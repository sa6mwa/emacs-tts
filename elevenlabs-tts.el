;;; elevenlabs-tts.el --- ElevenLabs Text-to-Speech integration for Emacs  -*- lexical-binding: t; -*-

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

(defcustom elevenlabs-tts-debug nil
  "Enable debug output for API requests.
When non-nil, shows detailed curl output and request information."
  :type 'boolean
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
Returns a filename like \\='basename-01.mp3\\=', \\='basename-02.mp3\\=', etc."
  (let ((counter 1)
        (filename))
    (while (progn
             (setq filename (format "%s-%02d.mp3" 
                                   (expand-file-name base-name base-path)
                                   counter))
             (file-exists-p filename))
      (setq counter (1+ counter)))
    filename))

(defun elevenlabs-tts--get-buffer-directory ()
  "Get the directory of the current buffer, or default directory if no file."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun elevenlabs-tts--get-base-filename ()
  "Get a base filename for the audio file based on current buffer."
  (if buffer-file-name
      (file-name-sans-extension (file-name-nondirectory buffer-file-name))
    "tts-audio"))

(defun elevenlabs-tts--make-api-request-sync (voice-id text filename)
  "Make synchronous API request to ElevenLabs with VOICE-ID and TEXT.
Save result to FILENAME and return success status."
  (let ((api-key (elevenlabs-tts--read-api-key)))
    (unless (and api-key (not (string-empty-p api-key)))
      (error "ElevenLabs API key not found or empty. Please check %s" elevenlabs-tts-api-key-file))
    
    ;; Try curl first for better UTF-8 handling, fall back to url package
    (if (executable-find "curl")
        (elevenlabs-tts--make-curl-request voice-id text filename api-key)
      (elevenlabs-tts--make-url-request voice-id text filename api-key))))

(defun elevenlabs-tts--make-curl-request (voice-id text filename api-key)
  "Make API request using curl for better UTF-8 handling.
  Uses temporary files for API key and data to avoid command-line exposure.
  Properly escapes filenames to prevent shell injection."
  (let* ((url (format "%s/text-to-speech/%s" elevenlabs-tts-api-base-url voice-id))
         (json-payload (json-encode
                        `((text . ,text)
                          (model_id . "eleven_monolingual_v1")
                          (voice_settings . ,elevenlabs-tts-default-settings))))
         (temp-json-file (make-temp-file "tts-json-" nil ".json"))
         (temp-header-file (make-temp-file "tts-header-" nil ".txt"))
         (temp-output (make-temp-file "tts-output-"))
         ;; Expand tilde and ensure full path for curl
         (expanded-filename (expand-file-name filename))
         ;; Escape filename for shell safety (though call-process should handle this)
         (safe-filename (shell-quote-argument expanded-filename))
         (exit-code))
    
    ;; Ensure output directory exists before curl writes to it
    (let ((output-dir (file-name-directory expanded-filename)))
      (when elevenlabs-tts-debug
        (message "Debug: Original filename: %s" filename)
        (message "Debug: Expanded filename: %s" expanded-filename)
        (message "Debug: Output directory: %s" output-dir)
        (message "Debug: Directory exists before mkdir: %s" (and output-dir (file-exists-p output-dir))))
      (when output-dir
        (unless (file-exists-p output-dir)
          (when elevenlabs-tts-debug
            (message "Debug: Creating directory: %s" output-dir))
          (make-directory output-dir t)
          (when elevenlabs-tts-debug
            (message "Debug: Directory exists after mkdir: %s" (file-exists-p output-dir))
            (message "Debug: Directory writable: %s" (file-writable-p output-dir))))))
    
    (unwind-protect
        (progn
          ;; Write JSON payload to temp file with proper UTF-8 encoding
          (with-temp-file temp-json-file
            (set-buffer-file-coding-system 'utf-8)
            (insert json-payload))
          
          ;; Write config file for curl to avoid API key in process list
          (let ((curl-config-file (make-temp-file "tts-curl-config-" nil ".txt")))
            (with-temp-file curl-config-file
              (insert (format "header = \"Content-Type: application/json; charset=utf-8\"\nheader = \"xi-api-key: %s\"\n" api-key)))
            
            (when elevenlabs-tts-debug
              (message "Debug: JSON payload: %s" json-payload)
              (message "Debug: Temp files - JSON: %s, Config: %s, Output: %s" temp-json-file curl-config-file temp-output))
            
            (message "Making request to ElevenLabs API with curl...")
            (setq exit-code
                  (call-process "curl" nil temp-output nil
                                "--config" curl-config-file
                                "-w" "HTTP_CODE:%{http_code}\n"
                                "-o" expanded-filename
                                "-X" "POST"
                                "--data-binary" (format "@%s" temp-json-file)
                                url))
            
            ;; Clean up config file immediately
            (when (file-exists-p curl-config-file)
              (delete-file curl-config-file)))
          
          ;; Read curl output for debugging
          (let ((curl-output (when (file-exists-p temp-output)
                              (with-temp-buffer
                                (insert-file-contents temp-output)
                                (buffer-string)))))
            
            (when elevenlabs-tts-debug
              (message "Debug: Curl exit code: %d" exit-code)
              (message "Debug: Curl output: %s" curl-output))
            
            (cond
             ((= exit-code 0)
              (if (and (file-exists-p expanded-filename) (> (nth 7 (file-attributes expanded-filename)) 100))
                  (progn
                    (message "✅ Audio successfully saved to: %s" filename)
                    t)
                (progn
                  (message "❌ No audio data received from API%s" 
                           (if elevenlabs-tts-debug
                               (format ". Curl output: %s" curl-output)
                             " (enable elevenlabs-tts-debug for details)"))
                  nil)))
             (t
              (let ((error-msg (cond
                               ((= exit-code 1) "Unsupported protocol or curl build issue")
                               ((= exit-code 2) "Failed to initialize curl")
                               ((= exit-code 3) "URL malformed")
                               ((= exit-code 6) "Couldn't resolve host (check internet connection)")
                               ((= exit-code 7) "Failed to connect to server")
                               ((= exit-code 22) "HTTP error (400+ status code)")
                               ((= exit-code 23) "Write error (couldn't write to output file)")
                               ((= exit-code 26) "Read error")
                               ((= exit-code 28) "Timeout")
                               (t (format "Unknown curl error (code %d)" exit-code)))))
                (message "❌ Curl request failed: %s%s" error-msg
                         (if elevenlabs-tts-debug
                             (format ". Output: %s" curl-output)
                           " (enable elevenlabs-tts-debug for details)"))
                nil)))))
      ;; Cleanup temp files
      (when (file-exists-p temp-json-file)
        (delete-file temp-json-file))
      (when (file-exists-p temp-header-file)
        (delete-file temp-header-file))
      (when (file-exists-p temp-output)
        (delete-file temp-output)))))

(defun elevenlabs-tts--make-url-request (voice-id text filename api-key)
  "Make API request using Emacs url package (fallback)."
  (let* ((url (format "%s/text-to-speech/%s" elevenlabs-tts-api-base-url voice-id))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("xi-api-key" . ,api-key)))
         ;; Ensure proper UTF-8 encoding for multibyte text
         (url-request-data
          (encode-coding-string
           (json-encode
            `((text . ,text)
              (model_id . "eleven_monolingual_v1")
              (voice_settings . ,elevenlabs-tts-default-settings)))
           'utf-8))
         (response-buffer))
      
      (condition-case error-data
          (progn
            (message "Making request to ElevenLabs API...")
            (setq response-buffer (url-retrieve-synchronously url t nil 30))
            
            (if response-buffer
                (with-current-buffer response-buffer
                  ;; Make sure we're at the beginning of the buffer
                  (goto-char (point-min))
                  (let ((first-line (buffer-substring-no-properties (point-min) (line-end-position))))
                    (if (string-match "HTTP/[0-9]+\\.[0-9]+ \\([0-9]+\\)" first-line)
                        (let ((status-code (string-to-number (match-string 1 first-line))))
                          (cond
                         ((= status-code 200)
                          ;; Find and extract the body
                          (if (search-forward "\n\n" nil t)
                              (let ((body-start (point))
                                    (body-size (- (point-max) (point))))
                                (if (> body-size 0)
                                    (progn
                                      ;; Save the audio data
                                      (let ((coding-system-for-write 'binary))
                                        (write-region body-start (point-max) filename))
                                      (kill-buffer response-buffer)
                                      (message "✅ Audio successfully saved to: %s" filename)
                                      t) ; Return success
                                  (progn
                                    (kill-buffer response-buffer)
                                    (message "❌ No audio data received from API")
                                    nil)))
                            (progn
                              (kill-buffer response-buffer)
                              (message "❌ Could not find response body")
                              nil)))
                         ((= status-code 401)
                          (kill-buffer response-buffer)
                          (message "❌ HTTP 401 - Unauthorized (check API key)")
                          nil)
                         ((= status-code 422)
                          (kill-buffer response-buffer)
                          (message "❌ HTTP 422 - Invalid request format")
                          nil)
                         (t
                          (kill-buffer response-buffer)
                          (message "❌ HTTP Error %d" status-code)
                          nil)))
                    (progn
                      (kill-buffer response-buffer)
                      (message "❌ Invalid HTTP response format")
                      nil))))
              (progn
                (message "❌ Failed to retrieve response from API")
                nil)))
        (error
         (when response-buffer (kill-buffer response-buffer))
         (message "❌ API request error: %s" (error-message-string error-data))
         nil))))


(defun elevenlabs-tts--select-voice (gender)
  "Select a voice based on GENDER (\='male or \='female)."
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
Interactive workflow: select voice, confirm/edit output path, generate audio."
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
      
      ;; Make synchronous API call  
      (elevenlabs-tts--make-api-request-sync voice-id text filename))))

;;;###autoload
(defun elevenlabs-tts-speak-selection-quick (gender)
  "Quick text-to-speech with predetermined GENDER voice.
GENDER should be \='male or \='female."
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
      
      ;; Make synchronous API call
      (elevenlabs-tts--make-api-request-sync voice-id text filename))))


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

;;;###autoload
(defun elevenlabs-tts-toggle-debug ()
  "Toggle debug output for ElevenLabs TTS."
  (interactive)
  (setq elevenlabs-tts-debug (not elevenlabs-tts-debug))
  (message "ElevenLabs TTS debug mode: %s" (if elevenlabs-tts-debug "enabled" "disabled")))

(provide 'elevenlabs-tts)

;;; elevenlabs-tts.el ends here
