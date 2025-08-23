;;; elevenlabs-tts.el --- ElevenLabs Text-to-Speech integration for Emacs

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, speech, audio
;; URL: https://github.com/sa6mwa/emacs-tts

;;; Commentary:

;; This package provides integration with ElevenLabs text-to-speech API.
;; It allows you to convert selected text to speech using high-quality voices
;; and saves the resulting audio file in the same directory as the current buffer.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)

(defgroup elevenlabs-tts nil
  "ElevenLabs Text-to-Speech integration."
  :group 'convenience
  :prefix "elevenlabs-tts-")

(defcustom elevenlabs-tts-api-key-file "~/.config/emacs-tts/elevenlabs-api-key"
  "Path to file containing your ElevenLabs API key.
The API key should be on the first line of this file.
Get an API key at https://elevenlabs.io/"
  :type 'string
  :group 'elevenlabs-tts)

(defcustom elevenlabs-tts-voices
  '(;; Female voices
    ("Rachel" . "21m00Tcm4TlvDq8ikWAM")    ; Female, American
    ("Domi" . "AZnzlk1XvdvUeBnXmlld")      ; Female, American
    ("Bella" . "EXAVITQu4vr4xnSDxMaL")     ; Female, American
    ("Elli" . "MF3mGyEYCl7XYWbV9V6O")      ; Female, American
    ("Emily" . "LcfcDJNUP1GQjkzn1xUU")     ; Female, American
    ("Matilda" . "XrExE9yKIg1WjnnlVkGX")   ; Female, American
    ("Grace" . "oWAxZDx7w5VEj9dCyTzz")     ; Female, American
    ("Serena" . "pMsXgVXv3BLzUgSXRplE")    ; Female, American
    ("Dorothy" . "ThT5KcBeYPX3keUQqHPh")   ; Female, British
    ("Charlotte" . "XB0fDUnXU5powFXDhCwa") ; Female, English
    ;; Male voices
    ("Drew" . "29vD33N1CtxCmqQRPOHJ")      ; Male, American
    ("Clyde" . "2EiwWnXFnvU5JabPnv8n")     ; Male, American
    ("Antoni" . "ErXwobaYiN019PkySvjV")    ; Male, American
    ("Josh" . "TxGEqnHWrfWFTfGW9XjX")      ; Male, American
    ("Arnold" . "VR6AewLTigWG4xSOukaG")    ; Male, American
    ("Adam" . "pNInz6obpgDQGcFmaJgB")      ; Male, American
    ("Sam" . "yoZ06aMxZJJ28mfd3POQ")       ; Male, American
    ("David" . "1iX6qVNKCsGGMEKdojN9")     ; Male, American
    ("Ethan" . "g5CIjZEefAph4nQFvHAz")     ; Male, American
    ("Brian" . "nPczCjzI2devNBz1zQrb")     ; Male, American
    ("Callum" . "N2lVS1w4EtoT3dr4eOWO")    ; Male, American
    ("Charlie" . "IKne3meq5aSn9XLyUdCD")   ; Male, Australian
    ("George" . "JBFqnCBsd6RMkjVDRZzb")    ; Male, English
    ("Liam" . "TX3LPaxmHKxFdv7VOQHJ"))     ; Male, American
  "Alist of voice names and their corresponding ElevenLabs voice IDs."
  :type '(alist :key-type string :value-type string)
  :group 'elevenlabs-tts)

(defcustom elevenlabs-tts-male-voices
  '("Drew" "Clyde" "Antoni" "Josh" "Arnold" "Adam" "Sam" "David" "Ethan" "Brian" "Callum" "Charlie" "George" "Liam")
  "List of male voice names."
  :type '(repeat string)
  :group 'elevenlabs-tts)

(defcustom elevenlabs-tts-female-voices
  '("Rachel" "Domi" "Bella" "Elli" "Emily" "Matilda" "Grace" "Serena" "Dorothy" "Charlotte")
  "List of female voice names."
  :type '(repeat string)
  :group 'elevenlabs-tts)

(defcustom elevenlabs-tts-default-settings
  '((stability . 0.5)
    (similarity_boost . 0.5)
    (style . 0.0)
    (use_speaker_boost . t))
  "Default voice settings for text-to-speech generation."
  :type '(alist :key-type symbol :value-type (choice number boolean))
  :group 'elevenlabs-tts)

(defvar elevenlabs-tts-api-base-url "https://api.elevenlabs.io/v1"
  "Base URL for ElevenLabs API.")

(defun elevenlabs-tts--read-api-key ()
  "Read API key from the configured file."
  (let ((key-file (expand-file-name elevenlabs-tts-api-key-file)))
    (if (file-exists-p key-file)
        (with-temp-buffer
          (insert-file-contents key-file)
          (string-trim (buffer-substring-no-properties (point-min) (line-end-position))))
      (error "API key file not found: %s. Please create this file with your ElevenLabs API key on the first line" key-file))))

(defun elevenlabs-tts--get-voice-id (voice-name)
  "Get the voice ID for VOICE-NAME."
  (cdr (assoc voice-name elevenlabs-tts-voices)))

(defun elevenlabs-tts--get-next-filename (base-path base-name)
  "Get the next sequential filename in BASE-PATH with BASE-NAME.
Returns a filename like 'basename-01.wav', 'basename-02.wav', etc."
  (let ((counter 1)
        (filename))
    (while (progn
             (setq filename (format "%s-%02d.wav" 
                                   (expand-file-name base-name base-path)
                                   counter))
             (file-exists-p filename))
      (setq counter (1+ counter)))
    filename))

(defun elevenlabs-tts--get-buffer-directory ()
  "Get the directory of the current buffer, or default directory if buffer has no file."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun elevenlabs-tts--get-base-filename ()
  "Get a base filename for the audio file based on current buffer."
  (if buffer-file-name
      (file-name-sans-extension (file-name-nondirectory buffer-file-name))
    "tts-audio"))

(defun elevenlabs-tts--make-api-request (voice-id text callback)
  "Make API request to ElevenLabs with VOICE-ID and TEXT, call CALLBACK with result."
  (let ((api-key (elevenlabs-tts--read-api-key)))
    (unless (and api-key (not (string-empty-p api-key)))
      (error "ElevenLabs API key not found or empty. Please check %s" elevenlabs-tts-api-key-file))
  
    (let* ((url (format "%s/text-to-speech/%s" elevenlabs-tts-api-base-url voice-id))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("xi-api-key" . ,api-key)))
         (url-request-data
          (json-encode
           `((text . ,text)
             (model_id . "eleven_monolingual_v1")
             (voice_settings . ,elevenlabs-tts-default-settings))))
           (buffer (url-retrieve url callback nil t)))
      buffer)))

(defun elevenlabs-tts--handle-api-response (status filename)
  "Handle API response with STATUS and save audio to FILENAME."
  (condition-case err
      (if (plist-get status :error)
          (progn
            (kill-buffer)
            (message "❌ API Error: %s" (plist-get status :error)))
        (progn
          ;; Check HTTP status
          (goto-char (point-min))
          (if (looking-at "HTTP/[0-9]\.[0-9] \([0-9]+\)")
              (let ((status-code (string-to-number (match-string 1))))
                (if (= status-code 200)
                    (progn
                      ;; Skip HTTP headers
                      (search-forward "\n\n")
                      (delete-region (point-min) (point))
                      
                      ;; Check if we got audio data
                      (if (= (point-min) (point-max))
                          (progn
                            (kill-buffer)
                            (message "❌ No audio data received from API"))
                        ;; Save the audio data
                        (let ((coding-system-for-write 'binary))
                          (write-region (point-min) (point-max) filename))
                        (kill-buffer)
                        (message "✅ Audio successfully saved to: %s" filename)))
                  (progn
                    (kill-buffer)
                    (message "❌ HTTP Error %d: Request failed" status-code))))
            (progn
              (kill-buffer)
              (message "❌ Invalid HTTP response received")))))
    (error
     (kill-buffer)
     (message "❌ Error processing response: %s" (error-message-string err)))))

(defun elevenlabs-tts--select-voice (gender)
  "Select a voice based on GENDER ('male or 'female)."
  (let* ((voice-list (if (eq gender 'male)
                        elevenlabs-tts-male-voices
                      elevenlabs-tts-female-voices))
         (voice-name (completing-read 
                     (format "Select %s voice: " gender)
                     voice-list nil t)))
    voice-name))

;;;###autoload
(defun elevenlabs-tts-speak-selection ()
  "Convert selected text to speech using ElevenLabs API.
Interactive workflow: select voice, confirm/edit output path, then generate audio."
  (interactive)
  (if (not (use-region-p))
      (message "Please select some text first")
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           ;; Combine all voices for selection
           (all-voices (append elevenlabs-tts-male-voices elevenlabs-tts-female-voices))
           (voice-name (completing-read "Select voice: " all-voices nil t))
           (voice-id (elevenlabs-tts--get-voice-id voice-name))
           (base-dir (elevenlabs-tts--get-buffer-directory))
           (base-name (elevenlabs-tts--get-base-filename))
           (default-filename (elevenlabs-tts--get-next-filename base-dir base-name))
           ;; Let user confirm or edit the output path
           (filename (read-file-name "Output file: " (file-name-directory default-filename)
                                    nil nil (file-name-nondirectory default-filename))))
      
      (unless voice-id
        (error "Voice ID not found for %s" voice-name))
      
      ;; Ensure the output directory exists
      (let ((output-dir (file-name-directory filename)))
        (unless (file-exists-p output-dir)
          (make-directory output-dir t)))
      
      (message "Generating speech with %s voice..." voice-name)
      
      (elevenlabs-tts--make-api-request 
       voice-id 
       text
       (lambda (status)
         (elevenlabs-tts--handle-api-response status filename))))))

;;;###autoload
(defun elevenlabs-tts-speak-selection-quick (gender)
  "Quick text-to-speech with predetermined GENDER voice.
GENDER should be 'male or 'female."
  (interactive 
   (list (intern (completing-read "Select gender: " '("male" "female") nil t))))
  (if (not (use-region-p))
      (message "Please select some text first")
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           (voice-list (if (eq gender 'male)
                          elevenlabs-tts-male-voices
                        elevenlabs-tts-female-voices))
           (voice-name (car voice-list))  ; Use first voice from list
           (voice-id (elevenlabs-tts--get-voice-id voice-name))
           (base-dir (elevenlabs-tts--get-buffer-directory))
           (base-name (elevenlabs-tts--get-base-filename))
           (filename (elevenlabs-tts--get-next-filename base-dir base-name)))
      
      (message "Generating speech with %s voice..." voice-name)
      
      (elevenlabs-tts--make-api-request 
       voice-id 
       text
       (lambda (status)
         (elevenlabs-tts--handle-api-response status filename))))))


;; Main keybinding
;;;###autoload
(defun elevenlabs-tts-setup-keybinding ()
  "Set up the main keybinding for elevenlabs-tts."
  (global-set-key (kbd "C-c s") 'elevenlabs-tts-speak-selection))

;; Convenient keybindings (optional)
;;;###autoload
(defun elevenlabs-tts-setup-keybindings ()
  "Set up convenient keybindings for elevenlabs-tts functions."
  (global-set-key (kbd "C-c s") 'elevenlabs-tts-speak-selection)
  (global-set-key (kbd "C-c S") (lambda () (interactive) (elevenlabs-tts-speak-selection-quick 'male)))
  (global-set-key (kbd "C-c M-s") (lambda () (interactive) (elevenlabs-tts-speak-selection-quick 'female))))

(provide 'elevenlabs-tts)

;;; elevenlabs-tts.el ends here
