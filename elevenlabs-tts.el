;;; elevenlabs-tts.el --- ElevenLabs Text-to-Speech integration for Emacs  -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, speech, audio
;; URL: https://github.com/sa6mwa/emacs-tts

;;; Commentary:

;; This package provides integration with ElevenLabs text-to-speech API.
;; It allows you to convert selected text to speech using high-quality voices
;; and saves the resulting audio file to a configurable output directory.
;;
;; The output directory can be set per buffer and defaults to the directory
;; of the current buffer's file. Use `elevenlabs-tts-set-output-directory'
;; to configure the output directory for a specific buffer.

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

(defcustom elevenlabs-tts-enable-playback t
  "Enable automatic playback prompt after TTS generation.
When non-nil, prompts user to play the generated audio file."
  :type 'boolean
  :group 'elevenlabs-tts)

(defcustom elevenlabs-tts-audio-players
  '(("ffplay" . ("-autoexit" "-nodisp"))
    ("mpg321" . ("-q"))
    ("mpg123" . ("-q"))
    ("mplayer" . ("-really-quiet" "-novideo"))
    ("mpv" . ("--no-video" "--really-quiet"))
    ("vlc" . ("--intf" "dummy" "--play-and-exit" "--quiet"))
    ("gst-play-1.0" . ("--quiet"))
    ("gst-launch-1.0" . ("playbin" "uri=file://")))
  "List of MP3-capable audio players with their command line options in order of preference.
Each entry is a cons cell where the car is the executable name
and the cdr is a list of command line arguments.
The first available player from this list will be used automatically."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'elevenlabs-tts)

(defvar elevenlabs-tts-api-base-url "https://api.elevenlabs.io/v1"
  "Base URL for ElevenLabs API.")

(defvar-local elevenlabs-tts-output-directory nil
  "Buffer-local output directory for TTS files.
When nil, defaults to the directory of the current buffer file.
This setting is maintained per buffer.")

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

(defun elevenlabs-tts--get-voice-name (voice-id)
  "Get the voice name for VOICE-ID."
  (let ((voice-entry (cl-find voice-id elevenlabs-tts-voices :key #'cdr :test #'string-equal)))
    (when voice-entry
      (car voice-entry))))

(defun elevenlabs-tts--get-next-filename (base-path base-name voice-name)
  "Get the next sequential filename in BASE-PATH with BASE-NAME and VOICE-NAME.
Returns a filename like \\='basename-0001-voicename.mp3\\=', \\='basename-0002-voicename.mp3\\=', etc.
Numbering is global across all voices, not per-voice."
  (let ((counter 1)
        (filename)
        (voice-name-lower (downcase voice-name))
        (base-pattern (expand-file-name base-name base-path)))
    ;; Find the highest existing number across ALL voices
    (let ((existing-files (directory-files (file-name-directory base-pattern) nil
                                          (format "^%s-[0-9]\\{4\\}-.*\\.mp3$" 
                                                 (regexp-quote (file-name-nondirectory base-pattern))))))
      (when existing-files
        (let ((max-number 0))
          (dolist (file existing-files)
            (when (string-match (format "^%s-\\([0-9]\\{4\\}\\)-.*\\.mp3$" 
                                       (regexp-quote (file-name-nondirectory base-pattern))) file)
              (let ((file-number (string-to-number (match-string 1 file))))
                (setq max-number (max max-number file-number)))))
          (setq counter (1+ max-number)))))
    
    ;; Generate filename with global counter
    (setq filename (format "%s-%04d-%s.mp3" 
                          base-pattern
                          counter
                          voice-name-lower))
    filename))

(defun elevenlabs-tts--get-buffer-directory ()
  "Get the directory of the current buffer, or default directory if no file."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun elevenlabs-tts--get-output-directory ()
  "Get the output directory for the current buffer.
Uses buffer-local setting if set, otherwise defaults to buffer directory."
  (or elevenlabs-tts-output-directory
      (elevenlabs-tts--get-buffer-directory)))

(defun elevenlabs-tts--set-output-directory (directory)
  "Set the output directory for the current buffer.
If DIRECTORY is an empty string, reset to default (buffer directory).
DIRECTORY should be a directory path, not a filename."
  (if (or (null directory) (string-empty-p directory))
      (setq elevenlabs-tts-output-directory nil)
    (let ((expanded-dir (expand-file-name directory)))
      ;; Ensure it ends with a directory separator
      (unless (string-suffix-p "/" expanded-dir)
        (setq expanded-dir (concat expanded-dir "/")))
      (setq elevenlabs-tts-output-directory expanded-dir))))

(defun elevenlabs-tts--prompt-for-output-directory ()
  "Prompt user for output directory, showing current setting.
Returns the selected directory, or nil if user wants default."
  (let* ((current-dir (elevenlabs-tts--get-output-directory))
         (default-dir (elevenlabs-tts--get-buffer-directory))
         (prompt (if (equal current-dir default-dir)
                     (format "Output directory (current: default - %s, empty for default): " default-dir)
                   (format "Output directory (current: %s, empty for default): " current-dir)))
         (selected-dir (read-directory-name prompt current-dir)))
    ;; Handle the case where user selects the same directory
    (if (equal (expand-file-name selected-dir) (expand-file-name current-dir))
        current-dir
      selected-dir)))

(defun elevenlabs-tts--get-base-filename ()
  "Get a base filename for the audio file based on current buffer."
  (if buffer-file-name
      (file-name-sans-extension (file-name-nondirectory buffer-file-name))
    "tts-audio"))

(defun elevenlabs-tts--find-available-audio-player ()
  "Find the first available audio player from the configured list.
Returns a cons cell (PLAYER-PATH . ARGS) or nil if none found."
  (let ((players elevenlabs-tts-audio-players)
        (result nil))
    (while (and players (not result))
      (let* ((player (car players))
             (player-name (car player))
             (player-args (cdr player))
             (player-path (executable-find player-name)))
        (when player-path
          (when elevenlabs-tts-debug
            (message "Debug: Found audio player: %s" player-name))
          (setq result (cons player-path player-args)))
        (setq players (cdr players))))
    result))

(defun elevenlabs-tts--play-audio-file (filename)
  "Play the audio file FILENAME using the best available player.
Returns t if playback was initiated successfully, nil otherwise."
  (let ((player-info (elevenlabs-tts--find-available-audio-player)))
    (if player-info
        (let* ((player-path (car player-info))
               (player-args (cdr player-info))
               (player-name (file-name-nondirectory player-path))
               (expanded-filename (expand-file-name filename))
               (command-args (append player-args (list expanded-filename))))
          
          (when elevenlabs-tts-debug
            (message "Debug: Playing with %s: %s %s" 
                     player-name player-path (mapconcat 'identity command-args " ")))
          
          ;; Special handling for gst-launch-1.0 which needs file:// URI
          (when (string-equal player-name "gst-launch-1.0")
            (setq command-args 
                  (list "playbin" (format "uri=file://%s" expanded-filename))))
          
          (condition-case error
              (progn
                (message "🔊 Playing audio with %s..." player-name)
                ;; Use call-process with full path to run the player and wait for it to finish
                ;; This ensures non-GUI players exit properly
                (let ((exit-code (apply 'call-process player-path nil nil nil command-args)))
                  (when elevenlabs-tts-debug
                    (message "Debug: Player %s exited with code: %d" player-name exit-code))
                  (if (= exit-code 0)
                      (progn
                        (message "✅ Playback completed")
                        t)
                    (progn
                      (message "⚠️  Player exited with code %d" exit-code)
                      t)))) ; Still return t as playback was attempted
            (error
             (message "❌ Error playing audio: %s" (error-message-string error))
             nil)))
      (progn
        (message "❌ No audio player available. Install one of: %s" 
                 (mapconcat (lambda (p) (car p)) elevenlabs-tts-audio-players ", "))
        nil))))

(defun elevenlabs-tts--is-valid-mp3-file (filename)
  "Check if FILENAME is a valid MP3 file by examining magic bytes.
Returns t if valid, nil otherwise."
  (when (and (file-exists-p filename) (> (nth 7 (file-attributes filename)) 10))
    (with-temp-buffer
      (set-buffer-multibyte nil)  ; Handle binary data
      (insert-file-contents-literally filename nil 0 10)  ; Read first 10 bytes
      (goto-char (point-min))
      ;; Check for MP3 magic bytes: ID3 tag ("ID3") or MP3 frame sync (0xFF 0xFB/0xFA/0xF3/0xF2)
      (or (looking-at "ID3")                              ; ID3v2 tag
          (and (= (following-char) #xFF)                   ; MP3 frame sync byte 1
               (progn (forward-char 1)
                      (memq (following-char) '(#xFB #xFA #xF3 #xF2))))))))  ; MP3 frame sync byte 2 variants

(defun elevenlabs-tts--parse-api-error-response (filename)
  "Try to parse FILENAME as a JSON error response from ElevenLabs API.
Returns error message string if it's a JSON error, nil if it's not JSON."
  (when (file-exists-p filename)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents filename)
          (goto-char (point-min))
          ;; Check if content looks like JSON (starts with { or [)
          (when (looking-at "[\\s-]*[{[]")
            (let* ((json-data (json-read))
                   (detail (cdr (assq 'detail json-data)))
                   (message (cdr (assq 'message json-data)))
                   (error-msg (cdr (assq 'error json-data))))
              ;; ElevenLabs returns different error formats
              (cond
               (detail detail)
               (message message) 
               (error-msg error-msg)
               (t "Unknown API error")))))
      (error nil))))

(defun elevenlabs-tts--extract-http-status (curl-output)
  "Extract HTTP status code from curl output containing HTTP_CODE: prefix.
Returns status code as integer or nil if not found."
  (when (and curl-output (string-match "HTTP_CODE:\\([0-9]+\\)" curl-output))
    (string-to-number (match-string 1 curl-output))))

(defun elevenlabs-tts--format-api-error-message (status-code error-detail)
  "Format a user-friendly error message based on STATUS-CODE and ERROR-DETAIL.
ERROR-DETAIL can be a string from API response or nil."
  (let ((base-message
         (cond
          ((= status-code 400) "Bad Request")
          ((= status-code 401) "Unauthorized (check your API key)")
          ((= status-code 402) "Payment Required (insufficient credits)")
          ((= status-code 403) "Forbidden (API key may lack permissions)")
          ((= status-code 404) "Not Found (voice or endpoint not found)")
          ((= status-code 422) "Unprocessable Entity (invalid request data)")
          ((= status-code 429) "Too Many Requests (rate limit exceeded)")
          ((= status-code 500) "Internal Server Error")
          ((= status-code 503) "Service Unavailable")
          ((>= status-code 400) (format "HTTP Error %d" status-code))
          (t "API Error"))))
    (if error-detail
        (format "%s: %s" base-message error-detail)
      base-message)))

(defun elevenlabs-tts--prompt-for-playback (filename)
  "Prompt user whether to play the audio file FILENAME.
Returns t if user wants to play, nil otherwise.
Defaults to 'N' (no), but 'y' + enter plays the audio."
  (when elevenlabs-tts-enable-playback
    (let ((available-player (elevenlabs-tts--find-available-audio-player)))
      (if available-player
          (let* ((player-path (car available-player))
                 (player-name (file-name-nondirectory player-path))
                 (prompt (format "🎵 Play audio with %s? [y/N]: " player-name))
                 (response (read-string prompt)))
            (cond
             ((string-match-p "^[yY]" response)
              (elevenlabs-tts--play-audio-file filename)
              t)
             (t 
              (message "Audio saved to: %s" filename)
              nil)))
        (progn
          (message "Audio saved to: %s (no player available for playback)" filename)
          nil)))))

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
              ;; Curl succeeded, now check HTTP status and validate response
              (let ((http-status (elevenlabs-tts--extract-http-status curl-output)))
                (if (and http-status (>= http-status 400))
                    ;; HTTP error - try to extract API error message
                    (let ((api-error (elevenlabs-tts--parse-api-error-response expanded-filename)))
                      (when elevenlabs-tts-debug
                        (message "Debug: HTTP error %d, API error: %s" http-status api-error))
                      ;; Clean up the invalid file
                      (when (file-exists-p expanded-filename)
                        (delete-file expanded-filename))
                      (message "❌ %s" (elevenlabs-tts--format-api-error-message http-status api-error))
                      nil)
                  ;; HTTP success (or status not detected) - validate file content
                  (if (and (file-exists-p expanded-filename) 
                           (> (nth 7 (file-attributes expanded-filename)) 10))
                      ;; Check if file is valid MP3 or contains error response
                      (if (elevenlabs-tts--is-valid-mp3-file expanded-filename)
                          (progn
                            (message "✅ Audio successfully saved to: %s" filename)
                            ;; Prompt for playback after successful generation
                            (elevenlabs-tts--prompt-for-playback filename)
                            t)
                        ;; File exists but isn't valid MP3 - might be JSON error
                        (let ((api-error (elevenlabs-tts--parse-api-error-response expanded-filename)))
                          (when (file-exists-p expanded-filename)
                            (delete-file expanded-filename))
                          (if api-error
                              (message "❌ API Error: %s" api-error)
                            (message "❌ Received invalid audio data (not MP3 format)"))
                          nil))
                    ;; File doesn't exist or is too small
                    (progn
                      (message "❌ No audio data received from API%s" 
                               (if elevenlabs-tts-debug
                                   (format ". Curl output: %s" curl-output)
                                 " (enable elevenlabs-tts-debug for details)"))
                      nil)))))
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
                                (if (> body-size 10)
                                    (progn
                                      ;; Save the response data
                                      (let ((coding-system-for-write 'binary))
                                        (write-region body-start (point-max) filename))
                                      (kill-buffer response-buffer)
                                      ;; Validate the saved file
                                      (if (elevenlabs-tts--is-valid-mp3-file filename)
                                          (progn
                                            (message "✅ Audio successfully saved to: %s" filename)
                                            ;; Prompt for playback after successful generation
                                            (elevenlabs-tts--prompt-for-playback filename)
                                            t) ; Return success
                                        ;; File isn't valid MP3 - check for API error
                                        (let ((api-error (elevenlabs-tts--parse-api-error-response filename)))
                                          (when (file-exists-p filename)
                                            (delete-file filename))
                                          (if api-error
                                              (message "❌ API Error: %s" api-error)
                                            (message "❌ Received invalid audio data (not MP3 format)"))
                                          nil)))
                                  (progn
                                    (kill-buffer response-buffer)
                                    (message "❌ No audio data received from API")
                                    nil)))
                            (progn
                              (kill-buffer response-buffer)
                              (message "❌ Could not find response body")
                              nil)))
                         ((>= status-code 400)
                          ;; HTTP error - try to extract error message from response body
                          (let ((api-error nil))
                            (if (search-forward "\n\n" nil t)
                                (let ((body-start (point))
                                      (temp-file (make-temp-file "tts-error-")))
                                  ;; Save error response to temp file for parsing
                                  (write-region body-start (point-max) temp-file)
                                  (setq api-error (elevenlabs-tts--parse-api-error-response temp-file))
                                  (when (file-exists-p temp-file)
                                    (delete-file temp-file))))
                            (kill-buffer response-buffer)
                            (message "❌ %s" (elevenlabs-tts--format-api-error-message status-code api-error))
                            nil))
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
  "Select a voice based on GENDER (\\='male or \\='female)."
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
Interactive workflow: select output directory, select voice, confirm/edit output path, generate audio."
  (interactive)
  (if (not (use-region-p))
      (message "Please select some text first")
    (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
           ;; Prompt for output directory and set it for this buffer
           (selected-output-dir (elevenlabs-tts--prompt-for-output-directory))
           (_ (elevenlabs-tts--set-output-directory selected-output-dir))
           ;; Now use the set output directory
           (output-dir (elevenlabs-tts--get-output-directory))
           ;; Combine all voices for selection
           (all-voices (append elevenlabs-tts-male-voices elevenlabs-tts-female-voices))
           (voice-name (completing-read "Select voice: " all-voices nil t))
           (voice-id (elevenlabs-tts--get-voice-id voice-name))
           (base-name (elevenlabs-tts--get-base-filename))
           (default-filename (elevenlabs-tts--get-next-filename output-dir base-name voice-name))
           ;; Let user confirm or edit the output path
           (filename (read-file-name "Output file: " (file-name-directory default-filename)
                                    nil nil (file-name-nondirectory default-filename))))
      
      (unless voice-id
        (error "Voice ID not found for %s" voice-name))
      
      ;; Ensure the output directory exists
      (let ((final-output-dir (file-name-directory filename)))
        (unless (file-exists-p final-output-dir)
          (make-directory final-output-dir t)))
      
      (message "Generating speech with %s voice..." voice-name)
      
      ;; Make synchronous API call  
      (elevenlabs-tts--make-api-request-sync voice-id text filename))))

;;;###autoload
(defun elevenlabs-tts-speak-selection-quick (gender)
  "Quick text-to-speech with predetermined GENDER voice.
GENDER should be \\='male or \\='female.
Uses the current buffer's output directory setting."
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
           ;; Use the per-buffer output directory setting
           (output-dir (elevenlabs-tts--get-output-directory))
           (base-name (elevenlabs-tts--get-base-filename))
           (filename (elevenlabs-tts--get-next-filename output-dir base-name voice-name)))
      
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
(defun elevenlabs-tts-set-output-directory ()
  "Set or view the output directory for the current buffer.
Prompts for a new directory. Enter empty string to reset to default."
  (interactive)
  (let* ((current-dir (elevenlabs-tts--get-output-directory))
         (default-dir (elevenlabs-tts--get-buffer-directory))
         (is-default (equal current-dir default-dir))
         (prompt (if is-default
                     (format "Output directory (currently default: %s, empty to keep default): " default-dir)
                   (format "Output directory (currently: %s, empty for default): " current-dir)))
         (input (read-directory-name prompt current-dir)))
    
    ;; Check if user wants to reset to default by entering empty string
    ;; (read-directory-name doesn't return empty string, so we check if it's the same as current)
    (if (equal (expand-file-name input) (expand-file-name current-dir))
        (message "Output directory unchanged: %s" current-dir)
      (progn
        (elevenlabs-tts--set-output-directory input)
        (let ((new-dir (elevenlabs-tts--get-output-directory))
              (new-is-default (equal (elevenlabs-tts--get-output-directory) default-dir)))
          (if new-is-default
              (message "Output directory reset to default: %s" new-dir)
            (message "Output directory set to: %s" new-dir)))))))

;;;###autoload
(defun elevenlabs-tts-toggle-debug ()
  "Toggle debug output for ElevenLabs TTS."
  (interactive)
  (setq elevenlabs-tts-debug (not elevenlabs-tts-debug))
  (message "ElevenLabs TTS debug mode: %s" (if elevenlabs-tts-debug "enabled" "disabled")))

;;;###autoload
(defun elevenlabs-tts-toggle-playback ()
  "Toggle automatic playback prompt after TTS generation."
  (interactive)
  (setq elevenlabs-tts-enable-playback (not elevenlabs-tts-enable-playback))
  (message "ElevenLabs TTS playback prompt: %s" (if elevenlabs-tts-enable-playback "enabled" "disabled")))

;;;###autoload
(defun elevenlabs-tts-test-audio-players ()
  "Test which audio players are available on the system."
  (interactive)
  (message "Testing available audio players...")
  (let ((available-players '())
        (unavailable-players '()))
    (dolist (player elevenlabs-tts-audio-players)
      (let ((player-name (car player)))
        (if (executable-find player-name)
            (push player-name available-players)
          (push player-name unavailable-players))))
    
    (if available-players
        (progn
          (message "Available audio players: %s" (mapconcat 'identity (reverse available-players) ", "))
          (when unavailable-players
            (message "Unavailable players: %s" (mapconcat 'identity (reverse unavailable-players) ", "))))
      (message "No audio players available! Install one of: %s" 
               (mapconcat (lambda (p) (car p)) elevenlabs-tts-audio-players ", ")))))

(provide 'elevenlabs-tts)

;;; elevenlabs-tts.el ends here
