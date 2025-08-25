;;; test-elevenlabs-tts.el --- Tests for ElevenLabs TTS package  -*- lexical-binding: t; -*-

;; Author: Test Suite
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (ert))

;;; Commentary:

;; This file contains tests for the ElevenLabs TTS package.
;; Run with: emacs -batch -l elevenlabs-tts.el -l test-elevenlabs-tts.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'elevenlabs-tts)

;;; Unit Tests

;; Regression tests for curl functionality and multibyte text handling

(ert-deftest test-elevenlabs-tts-get-voice-id ()
  "Test that voice ID lookup works for known voices."
  (should (equal (elevenlabs-tts--get-voice-id "Rachel") "21m00Tcm4TlvDq8ikWAM"))
  (should (equal (elevenlabs-tts--get-voice-id "Josh") "TxGEqnHWrfWFTfGW9XjX"))
  (should (null (elevenlabs-tts--get-voice-id "NonexistentVoice"))))

(ert-deftest test-elevenlabs-tts-get-voice-name ()
  "Test that voice name lookup works for known voice IDs."
  (should (equal (elevenlabs-tts--get-voice-name "21m00Tcm4TlvDq8ikWAM") "Rachel"))
  (should (equal (elevenlabs-tts--get-voice-name "TxGEqnHWrfWFTfGW9XjX") "Josh"))
  (should (null (elevenlabs-tts--get-voice-name "NonexistentVoiceID"))))

(ert-deftest test-elevenlabs-tts-get-base-filename ()
  "Test base filename generation."
  (let ((buffer-file-name "/home/user/documents/article.txt"))
    (should (equal (elevenlabs-tts--get-base-filename) "article")))
  (let ((buffer-file-name nil))
    (should (equal (elevenlabs-tts--get-base-filename) "tts-audio"))))

(ert-deftest test-elevenlabs-tts-get-next-filename ()
  "Test sequential filename generation with voice names and global numbering."
  (let ((temp-dir (make-temp-file "tts-test-" t)))
    (unwind-protect
        (progn
          ;; Test first file with 4-digit format
          (let ((filename (elevenlabs-tts--get-next-filename temp-dir "test" "Rachel")))
            (should (string-match "test-0001-rachel\\.mp3$" filename)))
          
          ;; Create the first file and test second with same voice
          (write-region "test" nil (expand-file-name "test-0001-rachel.mp3" temp-dir))
          (let ((filename (elevenlabs-tts--get-next-filename temp-dir "test" "Rachel")))
            (should (string-match "test-0002-rachel\\.mp3$" filename)))
          
          ;; Create the second file to establish the sequence
          (write-region "test" nil (expand-file-name "test-0002-rachel.mp3" temp-dir))
          
          ;; Test global numbering: different voice should continue sequence (not restart at 01)
          (let ((filename (elevenlabs-tts--get-next-filename temp-dir "test" "Josh")))
            (should (string-match "test-0003-josh\\.mp3$" filename)))
          
          ;; Create a higher numbered file to test max detection
          (write-region "test" nil (expand-file-name "test-0010-charlie.mp3" temp-dir))
          (let ((filename (elevenlabs-tts--get-next-filename temp-dir "test" "Emily")))
            (should (string-match "test-0011-emily\\.mp3$" filename))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-elevenlabs-tts-voice-lists ()
  "Test that voice lists are properly configured."
  (should (member "Rachel" elevenlabs-tts-female-voices))
  (should (member "Josh" elevenlabs-tts-male-voices))
  (should (> (length elevenlabs-tts-female-voices) 0))
  (should (> (length elevenlabs-tts-male-voices) 0)))

(ert-deftest test-elevenlabs-tts-default-settings ()
  "Test that default settings are properly configured."
  (should (assoc 'stability elevenlabs-tts-default-settings))
  (should (assoc 'similarity_boost elevenlabs-tts-default-settings))
  (should (assoc 'style elevenlabs-tts-default-settings))
  (should (assoc 'use_speaker_boost elevenlabs-tts-default-settings)))

(ert-deftest test-elevenlabs-tts-api-key-file-default ()
  "Test that API key file path is set correctly."
  (should (stringp elevenlabs-tts-api-key-file))
  (should (string-match "elevenlabs-api-key$" elevenlabs-tts-api-key-file)))

;;; Regression test for directory creation (curl exit code 23 fix)

(ert-deftest test-elevenlabs-tts-curl-creates-output-directory ()
  "Test that curl function creates output directory before writing (stubbed)."
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (tmp-root (make-temp-file "tts-curl-" t))
         (nested-dir (expand-file-name "a/b/c" tmp-root))
         (out-file (expand-file-name "result.mp3" nested-dir)))
    (unwind-protect
        (progn
          (should voice-id)
          ;; Stub call-process to simulate successful curl AND mock playback functions
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program infile buffer display &rest args)
                       ;; Extract output filename after "-o"
                       (let* ((o-index (cl-position "-o" args :test #'string=))
                              (ofile (and o-index (nth (1+ o-index) args))))
                         (when ofile
                           (with-temp-file ofile
                             (set-buffer-multibyte nil)
                             ;; Write valid MP3 magic bytes followed by some data
                             (insert "\xFF\xFB\x90\x00")
                             (insert (make-string 252 ?X))))
                         ;; Write HTTP 200 status to temp output for curl
                         (when buffer
                           (with-temp-buffer
                             (insert "HTTP_CODE:200\n")
                             (write-region (point-min) (point-max) buffer)))
                         0)))
                    ;; Mock playback prompt to avoid interaction
                    ((symbol-function 'elevenlabs-tts--prompt-for-playback)
                     (lambda (filename) nil)))
            (let ((result (elevenlabs-tts--make-curl-request voice-id
                                                             "Test text"
                                                             out-file
                                                             "DUMMY-KEY")))
              (should result)
              (should (file-exists-p out-file))
              (should (file-directory-p nested-dir)))))
      (when (file-exists-p tmp-root) 
        (delete-directory tmp-root t)))))

;;; Unit Tests for Playback Functionality

(defvar test-audio-file nil
  "Path to test audio file for testing.")

(defun test-elevenlabs-tts--create-test-audio-file ()
  "Create a test audio file for testing."
  (let ((test-file (make-temp-file "test-audio-" nil ".mp3")))
    ;; Create a minimal MP3-like file (need >10 bytes for validation)
    (with-temp-file test-file
      (set-buffer-multibyte nil)
      (insert (string 255 251 144 0))  ; MP3 frame sync bytes
      (insert (make-string 20 0)))     ; Add padding to make file >10 bytes
    (setq test-audio-file test-file)
    test-file))

(defun test-elevenlabs-tts--cleanup-test-audio-file ()
  "Clean up test audio file."
  (when (and test-audio-file (file-exists-p test-audio-file))
    (delete-file test-audio-file)
    (setq test-audio-file nil)))

(defun test-elevenlabs-tts--mock-executable-find (available-players)
  "Mock executable-find to return only specified AVAILABLE-PLAYERS.
AVAILABLE-PLAYERS should be a list of player names that should be 'found'."
  (let ((original-executable-find (symbol-function 'executable-find)))
    (fset 'executable-find 
          (lambda (command)
            (when (member command available-players)
              (format "/usr/bin/%s" command))))
    ;; Return function to restore original
    (lambda ()
      (fset 'executable-find original-executable-find))))

(ert-deftest test-elevenlabs-tts-find-available-audio-player-with-ffplay ()
  "Test finding ffplay as the first available player."
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay"))))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (consp result))
          (should (string-equal (car result) "/usr/bin/ffplay"))
          (should (equal (cdr result) '("-autoexit" "-nodisp"))))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-find-available-audio-player-with-mplayer ()
  "Test finding mplayer when ffplay is not available."
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '("mplayer"))))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (consp result))
          (should (string-equal (car result) "/usr/bin/mplayer"))
          (should (equal (cdr result) '("-really-quiet" "-novideo"))))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-find-available-audio-player-priority ()
  "Test player priority order (ffplay should be chosen over mplayer)."
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay" "mplayer" "vlc"))))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (consp result))
          (should (string-equal (car result) "/usr/bin/ffplay")))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-find-available-audio-player-none-found ()
  "Test when no audio player is found."
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '())))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (null result)))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-play-audio-file-success ()
  "Test successful audio file playback."
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (call-process-calls '())
        (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock call-process to record calls and return success
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (push (cons program args) call-process-calls)
                  0))  ; Return success
          
          (let ((result (elevenlabs-tts--play-audio-file test-file)))
            (should (eq result t))
            (should (= (length call-process-calls) 1))
            (let ((call (car call-process-calls)))
              (should (string-equal (car call) "/usr/bin/ffplay"))
              (should (member (expand-file-name test-file) (cdr call))))))
      
      (fset 'call-process original-call-process)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-play-audio-file-no-player ()
  "Test playback when no audio player is available."
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '())))
    (unwind-protect
        (let ((result (elevenlabs-tts--play-audio-file test-file)))
          (should (null result)))
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-prompt-for-playback-yes ()
  "Test playback prompt with 'y' input."
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (original-read-string (symbol-function 'read-string))
        (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock read-string to return 'y'
          (fset 'read-string (lambda (prompt) "y"))
          ;; Mock call-process to avoid actual playback
          (fset 'call-process (lambda (&rest args) 0))
          
          (let ((result (elevenlabs-tts--prompt-for-playback test-file)))
            (should (eq result t))))
      
      (fset 'read-string original-read-string)
      (fset 'call-process original-call-process)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-prompt-for-playback-no ()
  "Test playback prompt with 'n' input."
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (original-read-string (symbol-function 'read-string)))
    (unwind-protect
        (progn
          ;; Mock read-string to return 'n'
          (fset 'read-string (lambda (prompt) "n"))
          
          (let ((result (elevenlabs-tts--prompt-for-playback test-file)))
            (should (null result))))
      
      (fset 'read-string original-read-string)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-prompt-for-playback-disabled ()
  "Test playback prompt when playback is disabled."
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (original-enable-playback elevenlabs-tts-enable-playback))
    (unwind-protect
        (progn
          (setq elevenlabs-tts-enable-playback nil)
          (let ((result (elevenlabs-tts--prompt-for-playback test-file)))
            (should (null result))))
      
      (setq elevenlabs-tts-enable-playback original-enable-playback)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-toggle-playback ()
  "Test toggling playback functionality."
  (let ((original-state elevenlabs-tts-enable-playback))
    (unwind-protect
        (progn
          (elevenlabs-tts-toggle-playback)
          (should (not (eq elevenlabs-tts-enable-playback original-state)))
          (elevenlabs-tts-toggle-playback)
          (should (eq elevenlabs-tts-enable-playback original-state)))
      
      (setq elevenlabs-tts-enable-playback original-state))))

(ert-deftest test-elevenlabs-tts-audio-players-configuration ()
  "Test that audio players configuration is valid."
  (should (listp elevenlabs-tts-audio-players))
  (should (> (length elevenlabs-tts-audio-players) 0))
  
  ;; Check that each entry is a proper cons cell
  (dolist (player elevenlabs-tts-audio-players)
    (should (consp player))
    (should (stringp (car player)))
    (should (listp (cdr player))))
  
  ;; Check that ffplay is first (highest priority)
  (should (string-equal (caar elevenlabs-tts-audio-players) "ffplay")))

(ert-deftest test-elevenlabs-tts-gst-launch-special-handling ()
  "Test special handling for gst-launch-1.0."
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("gst-launch-1.0")))
        (call-process-calls '())
        (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock call-process to record calls
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (push (cons program args) call-process-calls)
                  0))
          
          (elevenlabs-tts--play-audio-file test-file)
          (should (= (length call-process-calls) 1))
          (let ((call (car call-process-calls)))
            (should (string-equal (car call) "/usr/bin/gst-launch-1.0"))
            (should (member "playbin" (cdr call)))
            (should (cl-some (lambda (arg) (string-prefix-p "uri=file://" arg)) (cdr call)))))
      
      (fset 'call-process original-call-process)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-test-audio-players-function ()
  "Test the test-audio-players interactive function."
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay" "mplayer"))))
    (unwind-protect
        ;; This should not error and should work with mocked players
        (progn
          (elevenlabs-tts-test-audio-players)
          (should t))  ; If we reach this point, no error occurred
      (funcall restore-fn))))

;;; Unit Tests for API Functions (with complete mocking)

(ert-deftest test-elevenlabs-tts-unit-api-request-success ()
  "Test successful API request with completely mocked backend (unit test)."
  ;; This is a true unit test - no real API calls, no real API key required
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-unit-success-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process))
         (original-read-api-key (symbol-function 'elevenlabs-tts--read-api-key)))
    (unwind-protect
        (progn
          ;; Mock API key reading to return dummy key
          (fset 'elevenlabs-tts--read-api-key
                (lambda () "sk-dummy-api-key-for-unit-test"))
          
          ;; Mock call-process to simulate successful curl with valid MP3 response
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (when (and (string-equal program "curl")
                             (cl-find "-o" args :test #'string=))
                    ;; Create valid MP3 file
                    (let* ((o-index (cl-position "-o" args :test #'string=))
                           (ofile (and o-index (nth (1+ o-index) args))))
                      (when ofile
                        (with-temp-file ofile
                          (set-buffer-multibyte nil)
                          (insert (string 255 251 144 0))  ; MP3 frame sync
                          (insert (make-string 1000 0)))))
                    ;; Write successful HTTP status
                    (when destination
                      (with-temp-buffer
                        (insert "HTTP_CODE:200\n")
                        (write-region (point-min) (point-max) destination))))
                  0))  ; Return success
          
          ;; Mock playback prompt
          (cl-letf (((symbol-function 'elevenlabs-tts--prompt-for-playback)
                     (lambda (filename) nil)))
            (let ((result (elevenlabs-tts--make-api-request-sync voice-id "Unit test text" temp-file)))
              (should result)
              (should (file-exists-p temp-file))
              (should (> (nth 7 (file-attributes temp-file)) 100)))))
      
      ;; Restore original functions
      (fset 'call-process original-call-process)
      (fset 'elevenlabs-tts--read-api-key original-read-api-key)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-elevenlabs-tts-unit-api-request-http-error ()
  "Test API request with HTTP error response (unit test)."
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-unit-error-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process))
         (original-read-api-key (symbol-function 'elevenlabs-tts--read-api-key)))
    (unwind-protect
        (progn
          ;; Mock API key reading
          (fset 'elevenlabs-tts--read-api-key
                (lambda () "sk-dummy-api-key-for-unit-test"))
          
          ;; Mock call-process to simulate HTTP 402 error
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (when (and (string-equal program "curl")
                             (cl-find "-o" args :test #'string=))
                    ;; Create JSON error response
                    (let* ((o-index (cl-position "-o" args :test #'string=))
                           (ofile (and o-index (nth (1+ o-index) args))))
                      (when ofile
                        (with-temp-file ofile
                          (insert "{\"detail\": \"Unit test: insufficient credits\"}"))))
                    ;; Write error HTTP status
                    (when destination
                      (with-temp-buffer
                        (insert "HTTP_CODE:402\n")
                        (write-region (point-min) (point-max) destination))))
                  0))  ; Curl itself succeeds
          
          (let ((result (elevenlabs-tts--make-api-request-sync voice-id "Unit test text" temp-file)))
            (should (null result))  ; Should fail due to HTTP error
            (should-not (file-exists-p temp-file))))  ; Error file should be cleaned up
      
      ;; Restore original functions
      (fset 'call-process original-call-process)
      (fset 'elevenlabs-tts--read-api-key original-read-api-key)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-elevenlabs-tts-unit-api-request-no-api-key ()
  "Test API request failure when no API key is available (unit test)."
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-unit-nokey-" nil ".mp3"))
         (original-read-api-key (symbol-function 'elevenlabs-tts--read-api-key)))
    (unwind-protect
        (progn
          ;; Mock API key reading to simulate missing key file
          (fset 'elevenlabs-tts--read-api-key
                (lambda () (error "API key file not found: unit test simulation")))
          
          (should-error (elevenlabs-tts--make-api-request-sync voice-id "Unit test text" temp-file)))
      
      ;; Restore original function
      (fset 'elevenlabs-tts--read-api-key original-read-api-key)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-elevenlabs-tts-unit-api-request-invalid-response ()
  "Test API request with invalid MP3 response (unit test)."
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-unit-invalid-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process))
         (original-read-api-key (symbol-function 'elevenlabs-tts--read-api-key)))
    (unwind-protect
        (progn
          ;; Mock API key reading
          (fset 'elevenlabs-tts--read-api-key
                (lambda () "sk-dummy-api-key-for-unit-test"))
          
          ;; Mock call-process to simulate HTTP 200 but with invalid MP3 data
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (when (and (string-equal program "curl")
                             (cl-find "-o" args :test #'string=))
                    ;; Create invalid response (text instead of MP3)
                    (let* ((o-index (cl-position "-o" args :test #'string=))
                           (ofile (and o-index (nth (1+ o-index) args))))
                      (when ofile
                        (with-temp-file ofile
                          (insert "This is not an MP3 file - unit test"))))
                    ;; Write successful HTTP status
                    (when destination
                      (with-temp-buffer
                        (insert "HTTP_CODE:200\n")
                        (write-region (point-min) (point-max) destination))))
                  0))
          
          (let ((result (elevenlabs-tts--make-api-request-sync voice-id "Unit test text" temp-file)))
            (should (null result))  ; Should fail due to invalid MP3
            (should-not (file-exists-p temp-file))))  ; Invalid file should be cleaned up
      
      ;; Restore original functions
      (fset 'call-process original-call-process)
      (fset 'elevenlabs-tts--read-api-key original-read-api-key)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Error Handling Tests

(ert-deftest test-elevenlabs-tts-is-valid-mp3-file-valid ()
  "Test MP3 file validation with valid MP3 data."
  (let ((test-file (make-temp-file "test-mp3-" nil ".mp3")))
    (unwind-protect
        (progn
          ;; Write valid ID3v2 header with enough data (need >10 bytes)
          (with-temp-file test-file
            (set-buffer-multibyte nil)
            ;; ID3v2 header: "ID3" + version + flags + size (10 bytes) + some data
            (insert (string ?I ?D ?3 3 0 0 0 0 0 20)) ; ID3v2.3, size=20
            (insert (make-string 20 0))) ; Add 20 bytes of padding
          (should (elevenlabs-tts--is-valid-mp3-file test-file)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-elevenlabs-tts-is-valid-mp3-file-frame-sync ()
  "Test MP3 file validation with MP3 frame sync bytes."
  (let ((test-file (make-temp-file "test-mp3-" nil ".mp3")))
    (unwind-protect
        (progn
          ;; Write MP3 frame sync bytes with enough data (need >10 bytes)
          (with-temp-file test-file
            (set-buffer-multibyte nil)
            (insert (string 255 251 144 0))  ; MP3 frame sync bytes
            (insert (make-string 20 0)))     ; Add padding to make file >10 bytes
          (should (elevenlabs-tts--is-valid-mp3-file test-file)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-elevenlabs-tts-is-valid-mp3-file-invalid ()
  "Test MP3 file validation with invalid data."
  (let ((test-file (make-temp-file "test-json-" nil ".mp3")))
    (unwind-protect
        (progn
          ;; Write JSON data (not MP3)
          (with-temp-file test-file
            (insert "{\"error\": \"Invalid request\"}"))
          (should-not (elevenlabs-tts--is-valid-mp3-file test-file)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-elevenlabs-tts-parse-api-error-response ()
  "Test parsing JSON error responses from ElevenLabs API."
  (let ((test-file (make-temp-file "test-error-" nil ".json")))
    (unwind-protect
        (progn
          ;; Test detail field
          (with-temp-file test-file
            (insert "{\"detail\": \"Insufficient credits\"}"))
          (should (string-equal (elevenlabs-tts--parse-api-error-response test-file)
                               "Insufficient credits"))
          
          ;; Test message field
          (with-temp-file test-file
            (insert "{\"message\": \"Invalid API key\"}"))
          (should (string-equal (elevenlabs-tts--parse-api-error-response test-file)
                               "Invalid API key"))
          
          ;; Test error field
          (with-temp-file test-file
            (insert "{\"error\": \"Rate limit exceeded\"}"))
          (should (string-equal (elevenlabs-tts--parse-api-error-response test-file)
                               "Rate limit exceeded"))
          
          ;; Test non-JSON content
          (with-temp-file test-file
            (insert "Not JSON content"))
          (should (null (elevenlabs-tts--parse-api-error-response test-file))))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest test-elevenlabs-tts-extract-http-status ()
  "Test extracting HTTP status codes from curl output."
  (should (= (elevenlabs-tts--extract-http-status "HTTP_CODE:200") 200))
  (should (= (elevenlabs-tts--extract-http-status "Some text\nHTTP_CODE:402\nmore text") 402))
  (should (= (elevenlabs-tts--extract-http-status "HTTP_CODE:404") 404))
  (should (null (elevenlabs-tts--extract-http-status "No HTTP code here"))))

(ert-deftest test-elevenlabs-tts-format-api-error-message ()
  "Test formatting user-friendly error messages."
  (should (string-equal (elevenlabs-tts--format-api-error-message 402 "Insufficient credits")
                       "Payment Required (insufficient credits): Insufficient credits"))
  (should (string-equal (elevenlabs-tts--format-api-error-message 401 nil)
                       "Unauthorized (check your API key)"))
  (should (string-equal (elevenlabs-tts--format-api-error-message 429 "Too many requests")
                       "Too Many Requests (rate limit exceeded): Too many requests"))
  (should (string-equal (elevenlabs-tts--format-api-error-message 500 nil)
                       "Internal Server Error")))

(ert-deftest test-elevenlabs-tts-unit-curl-error-handling-http-error ()
  "Test curl error handling with HTTP error response (mocked unit test)."
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-error-test-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock call-process to simulate HTTP 402 error with JSON response
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (when (and (string-equal program "curl")
                             (cl-find "-o" args :test #'string=))
                    ;; Write error JSON to output file
                    (let* ((o-index (cl-position "-o" args :test #'string=))
                           (ofile (and o-index (nth (1+ o-index) args))))
                      (when ofile
                        (with-temp-file ofile
                          (insert "{\"detail\": \"You have insufficient credits to complete this request\"}"))))
                    ;; Write HTTP status to temp output
                    (when destination
                      (with-temp-buffer
                        (insert "HTTP_CODE:402\n")
                        (write-region (point-min) (point-max) destination))))
                  0))  ; Return success (curl succeeded, but HTTP error)
          
          (let ((result (elevenlabs-tts--make-curl-request voice-id "Test text" temp-file "DUMMY-KEY")))
            (should (null result))  ; Should fail due to HTTP error
            (should-not (file-exists-p temp-file))))  ; Error file should be cleaned up
      
      (fset 'call-process original-call-process)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-elevenlabs-tts-unit-curl-error-handling-invalid-mp3 ()
  "Test curl error handling when response is not valid MP3 (mocked unit test)."
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-invalid-test-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock call-process to return JSON instead of MP3
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (when (and (string-equal program "curl")
                             (cl-find "-o" args :test #'string=))
                    ;; Write JSON error to output file (should be MP3)
                    (let* ((o-index (cl-position "-o" args :test #'string=))
                           (ofile (and o-index (nth (1+ o-index) args))))
                      (when ofile
                        (with-temp-file ofile
                          (insert "{\"detail\": \"Some API error\"}"))))
                    ;; Write HTTP 200 status to temp output
                    (when destination
                      (with-temp-buffer
                        (insert "HTTP_CODE:200\n")
                        (write-region (point-min) (point-max) destination))))
                  0))
          
          (let ((result (elevenlabs-tts--make-curl-request voice-id "Test text" temp-file "DUMMY-KEY")))
            (should (null result))  ; Should fail due to invalid MP3
            (should-not (file-exists-p temp-file))))  ; Invalid file should be cleaned up
      
      (fset 'call-process original-call-process)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Integration Tests (require API key)

(ert-deftest test-elevenlabs-tts-integration-api-key-reading ()
  "Test reading API key from file (integration test)."
  :tags '(integration)
  (let ((api-key-file (expand-file-name elevenlabs-tts-api-key-file)))
    (if (file-exists-p api-key-file)
        (should (stringp (elevenlabs-tts--read-api-key)))
      (should-error (elevenlabs-tts--read-api-key)))))

(ert-deftest test-elevenlabs-tts-integration-voice-generation ()
  "Test actual voice generation with API (integration test).

This test requires:
- Valid API key in ~/.config/emacs-tts/elevenlabs-api-key
- Sufficient credits in ElevenLabs account
- Network connectivity

The test will FAIL if API returns errors (insufficient credits, invalid key, etc.)."
  :tags '(integration)
  (skip-unless (file-exists-p (expand-file-name elevenlabs-tts-api-key-file)))
  (let* ((temp-dir (make-temp-file "tts-integration-" t))
         (output-file (expand-file-name "integration-test.mp3" temp-dir))
         (voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (test-text "This is a test."))
    (unwind-protect
        (progn
          (should voice-id)
          ;; Mock playback prompt to avoid interaction during tests
          (cl-letf (((symbol-function 'elevenlabs-tts--prompt-for-playback)
                     (lambda (filename) nil)))
            (let ((result (elevenlabs-tts--make-api-request-sync voice-id test-text output-file)))
              ;; Integration test should FAIL if API call fails
              (should result) ; This will fail the test if API returns error
              (should (file-exists-p output-file))
              (should (> (nth 7 (file-attributes output-file)) 1000)))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'test-elevenlabs-tts)

;;; test-elevenlabs-tts.el ends here
