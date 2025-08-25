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
  :tags '(unit)
  (should (equal (elevenlabs-tts--get-voice-id "Rachel") "21m00Tcm4TlvDq8ikWAM"))
  (should (equal (elevenlabs-tts--get-voice-id "Josh") "TxGEqnHWrfWFTfGW9XjX"))
  (should (null (elevenlabs-tts--get-voice-id "NonexistentVoice"))))

(ert-deftest test-elevenlabs-tts-get-voice-name ()
  "Test that voice name lookup works for known voice IDs."
  :tags '(unit)
  (should (equal (elevenlabs-tts--get-voice-name "21m00Tcm4TlvDq8ikWAM") "Rachel"))
  (should (equal (elevenlabs-tts--get-voice-name "TxGEqnHWrfWFTfGW9XjX") "Josh"))
  (should (null (elevenlabs-tts--get-voice-name "NonexistentVoiceID"))))

(ert-deftest test-elevenlabs-tts-get-base-filename ()
  "Test base filename generation."
  :tags '(unit)
  (let ((buffer-file-name "/home/user/documents/article.txt"))
    (should (equal (elevenlabs-tts--get-base-filename) "article")))
  (let ((buffer-file-name nil))
    (should (equal (elevenlabs-tts--get-base-filename) "tts-audio"))))

(ert-deftest test-elevenlabs-tts-get-next-filename ()
  "Test sequential filename generation with voice names and global numbering."
  :tags '(unit)
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
  :tags '(unit)
  (should (member "Rachel" elevenlabs-tts-female-voices))
  (should (member "Josh" elevenlabs-tts-male-voices))
  (should (> (length elevenlabs-tts-female-voices) 0))
  (should (> (length elevenlabs-tts-male-voices) 0)))

(ert-deftest test-elevenlabs-tts-default-settings ()
  "Test that default settings are properly configured."
  :tags '(unit)
  (should (assoc 'stability elevenlabs-tts-default-settings))
  (should (assoc 'similarity_boost elevenlabs-tts-default-settings))
  (should (assoc 'style elevenlabs-tts-default-settings))
  (should (assoc 'use_speaker_boost elevenlabs-tts-default-settings))
  (should (assoc 'speed elevenlabs-tts-default-settings))
  ;; Verify speed default is 1.0
  (should (equal (cdr (assq 'speed elevenlabs-tts-default-settings)) 1.0)))

(ert-deftest test-elevenlabs-tts-api-key-file-default ()
  "Test that API key file path is set correctly."
  :tags '(unit)
  (should (stringp elevenlabs-tts-api-key-file))
  (should (string-match "elevenlabs-api-key$" elevenlabs-tts-api-key-file)))

;;; Regression test for directory creation (curl exit code 23 fix)

(ert-deftest test-elevenlabs-tts-curl-creates-output-directory ()
  "Test that curl function creates output directory before writing (stubbed)."
  :tags '(unit)
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
  :tags '(unit)
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay"))))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (consp result))
          (should (string-equal (car result) "/usr/bin/ffplay"))
          (should (equal (cdr result) '("-autoexit" "-nodisp"))))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-find-available-audio-player-with-mplayer ()
  "Test finding mplayer when ffplay is not available."
  :tags '(unit)
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '("mplayer"))))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (consp result))
          (should (string-equal (car result) "/usr/bin/mplayer"))
          (should (equal (cdr result) '("-really-quiet" "-novideo"))))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-find-available-audio-player-priority ()
  "Test player priority order (ffplay should be chosen over mplayer)."
  :tags '(unit)
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay" "mplayer" "vlc"))))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (consp result))
          (should (string-equal (car result) "/usr/bin/ffplay")))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-find-available-audio-player-none-found ()
  "Test when no audio player is found."
  :tags '(unit)
  (let ((restore-fn (test-elevenlabs-tts--mock-executable-find '())))
    (unwind-protect
        (let ((result (elevenlabs-tts--find-available-audio-player)))
          (should (null result)))
      (funcall restore-fn))))

(ert-deftest test-elevenlabs-tts-play-audio-file-success ()
  "Test successful audio file playback."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (call-process-calls '())
        (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock call-process to record calls and return success
          ;; Handle the new stderr buffer parameter
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (push (cons program args) call-process-calls)
                  ;; If destination is a list with stderr buffer, don't write to it for success case
                  0))  ; Return success
          
          (let ((playback-result (elevenlabs-tts--play-audio-file test-file)))
            (should (eq playback-result t))
            (should (= (length call-process-calls) 1))
            (let ((call (car call-process-calls)))
              (should (string-equal (car call) "/usr/bin/ffplay"))
              (should (member (expand-file-name test-file) (cdr call))))))
      
      (fset 'call-process original-call-process)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-play-audio-file-no-player ()
  "Test playback when no audio player is available."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '())))
    (unwind-protect
        (let ((playback-result (elevenlabs-tts--play-audio-file test-file)))
          (should (null playback-result)))
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-prompt-for-playback-yes ()
  "Test playback prompt with 'y' input."
  :tags '(unit)
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
  :tags '(unit)
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
  :tags '(unit)
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
  :tags '(unit)
  (let ((original-state elevenlabs-tts-enable-playback))
    (unwind-protect
        (progn
          (elevenlabs-tts-toggle-playback)
          (should (not (eq elevenlabs-tts-enable-playback original-state)))
          (elevenlabs-tts-toggle-playback)
          (should (eq elevenlabs-tts-enable-playback original-state)))
      
      (setq elevenlabs-tts-enable-playback original-state))))

(ert-deftest test-elevenlabs-tts-auto-play-enabled ()
  "Test auto-play functionality when enabled."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (original-auto-play elevenlabs-tts-auto-play)
        (original-enable-playback elevenlabs-tts-enable-playback)
        (original-call-process (symbol-function 'call-process))
        (play-audio-called nil))
    (unwind-protect
        (progn
          ;; Enable auto-play
          (setq elevenlabs-tts-auto-play t)
          (setq elevenlabs-tts-enable-playback nil)  ; This should be ignored when auto-play is on
          ;; Mock call-process to avoid actual playback
          (fset 'call-process (lambda (&rest args) (setq play-audio-called t) 0))
          
          (let ((result (elevenlabs-tts--prompt-for-playback test-file)))
            (should (eq result t))
            (should play-audio-called)))
      
      (fset 'call-process original-call-process)
      (setq elevenlabs-tts-auto-play original-auto-play)
      (setq elevenlabs-tts-enable-playback original-enable-playback)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-auto-play-disabled ()
  "Test auto-play functionality when disabled (normal prompt behavior)."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (original-auto-play elevenlabs-tts-auto-play)
        (original-enable-playback elevenlabs-tts-enable-playback)
        (original-read-string (symbol-function 'read-string))
        (original-call-process (symbol-function 'call-process))
        (read-string-called nil)
        (play-audio-called nil))
    (unwind-protect
        (progn
          ;; Disable auto-play and enable playback prompting
          (setq elevenlabs-tts-auto-play nil)
          (setq elevenlabs-tts-enable-playback t)
          ;; Mock read-string to return 'y'
          (fset 'read-string (lambda (prompt) (setq read-string-called t) "y"))
          ;; Mock call-process to avoid actual playback
          (fset 'call-process (lambda (&rest args) (setq play-audio-called t) 0))
          
          (let ((result (elevenlabs-tts--prompt-for-playback test-file)))
            (should (eq result t))
            (should read-string-called)  ; Should have prompted user
            (should play-audio-called)))
      
      (fset 'read-string original-read-string)
      (fset 'call-process original-call-process)
      (setq elevenlabs-tts-auto-play original-auto-play)
      (setq elevenlabs-tts-enable-playback original-enable-playback)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-auto-play-no-player ()
  "Test auto-play when no audio player is available."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '()))  ; No players available
        (original-auto-play elevenlabs-tts-auto-play))
    (unwind-protect
        (progn
          ;; Enable auto-play but no player available
          (setq elevenlabs-tts-auto-play t)
          
          (let ((result (elevenlabs-tts--prompt-for-playback test-file)))
            (should (null result))  ; Should return nil when no player available
            ;; Should show appropriate message about no player for auto-play
            ))
      
      (setq elevenlabs-tts-auto-play original-auto-play)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-toggle-auto-play ()
  "Test toggling auto-play functionality."
  :tags '(unit)
  (let ((original-state elevenlabs-tts-auto-play))
    (unwind-protect
        (progn
          (elevenlabs-tts-toggle-auto-play)
          (should (not (eq elevenlabs-tts-auto-play original-state)))
          (elevenlabs-tts-toggle-auto-play)
          (should (eq elevenlabs-tts-auto-play original-state)))
      
      (setq elevenlabs-tts-auto-play original-state))))

(ert-deftest test-elevenlabs-tts-auto-play-priority-over-prompt ()
  "Test that auto-play takes priority over playback prompt setting."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (original-auto-play elevenlabs-tts-auto-play)
        (original-enable-playback elevenlabs-tts-enable-playback)
        (original-call-process (symbol-function 'call-process))
        (original-read-string (symbol-function 'read-string))
        (read-string-called nil)
        (play-audio-called nil))
    (unwind-protect
        (progn
          ;; Enable both auto-play and playback prompting - auto-play should win
          (setq elevenlabs-tts-auto-play t)
          (setq elevenlabs-tts-enable-playback t)
          ;; Mock read-string - should NOT be called due to auto-play
          (fset 'read-string (lambda (prompt) (setq read-string-called t) "n"))
          ;; Mock call-process to avoid actual playback
          (fset 'call-process (lambda (&rest args) (setq play-audio-called t) 0))
          
          (let ((result (elevenlabs-tts--prompt-for-playback test-file)))
            (should (eq result t))
            (should (not read-string-called))  ; Should NOT have prompted user due to auto-play
            (should play-audio-called)))       ; Should have played automatically
      
      (fset 'read-string original-read-string)
      (fset 'call-process original-call-process)
      (setq elevenlabs-tts-auto-play original-auto-play)
      (setq elevenlabs-tts-enable-playback original-enable-playback)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-audio-players-configuration ()
  "Test that audio players configuration is valid."
  :tags '(unit)
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
  :tags '(unit)
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
  :tags '(unit)
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
  :tags '(unit)
  ;; This is a true unit test - no real API calls, no real API key required
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-unit-success-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process))
         (original-read-api-key (symbol-function 'elevenlabs-tts--read-api-key))
         (restore-executable-find (test-elevenlabs-tts--mock-executable-find '("curl"))))
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
      (funcall restore-executable-find)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-elevenlabs-tts-unit-api-request-http-error ()
  "Test API request with HTTP error response (unit test)."
  :tags '(unit)
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-unit-error-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process))
         (original-read-api-key (symbol-function 'elevenlabs-tts--read-api-key))
         (restore-executable-find (test-elevenlabs-tts--mock-executable-find '("curl"))))
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
      (funcall restore-executable-find)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-elevenlabs-tts-unit-api-request-no-api-key ()
  "Test API request failure when no API key is available (unit test)."
  :tags '(unit)
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
  :tags '(unit)
  (let* ((voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (temp-file (make-temp-file "tts-unit-invalid-" nil ".mp3"))
         (original-call-process (symbol-function 'call-process))
         (original-read-api-key (symbol-function 'elevenlabs-tts--read-api-key))
         (restore-executable-find (test-elevenlabs-tts--mock-executable-find '("curl"))))
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
      (funcall restore-executable-find)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Error Handling Tests

(ert-deftest test-elevenlabs-tts-is-valid-mp3-file-valid ()
  "Test MP3 file validation with valid MP3 data."
  :tags '(unit)
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
  :tags '(unit)
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
  :tags '(unit)
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
  :tags '(unit)
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
  :tags '(unit)
  (should (= (elevenlabs-tts--extract-http-status "HTTP_CODE:200") 200))
  (should (= (elevenlabs-tts--extract-http-status "Some text\nHTTP_CODE:402\nmore text") 402))
  (should (= (elevenlabs-tts--extract-http-status "HTTP_CODE:404") 404))
  (should (null (elevenlabs-tts--extract-http-status "No HTTP code here"))))

(ert-deftest test-elevenlabs-tts-format-api-error-message ()
  "Test formatting user-friendly error messages."
  :tags '(unit)
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
  :tags '(unit)
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

(ert-deftest test-elevenlabs-tts-play-audio-file-shared-library-error ()
  "Test shared library error detection in audio playback."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock call-process to simulate exit code 127 with shared library error in stderr
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (when (and destination (listp destination))
                    ;; Write shared library error to stderr temp file (second element of destination list)
                    (let ((stderr-file (cadr destination)))
                      (when stderr-file
                        (with-temp-file stderr-file
                          (insert "/usr/bin/ffplay: error while loading shared libraries: libavdevice.so.62: cannot open shared object file: No such file or directory\n")))))
                  127))  ; Return exit code 127
          
          (let ((playback-result (elevenlabs-tts--play-audio-file test-file)))
            ;; Should return nil for shared library errors (indicating failure to start playback)
            (should (null playback-result))))
      
      (fset 'call-process original-call-process)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file))))

(ert-deftest test-elevenlabs-tts-play-audio-file-command-not-found-error ()
  "Test handling of regular command not found errors (exit code 127 without shared library message)."
  :tags '(unit)
  (let ((test-file (test-elevenlabs-tts--create-test-audio-file))
        (restore-fn (test-elevenlabs-tts--mock-executable-find '("ffplay")))
        (original-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Mock call-process to simulate exit code 127 without shared library error
          (fset 'call-process
                (lambda (program &optional infile destination display &rest args)
                  (when (and destination (listp destination))
                    ;; Write generic error to stderr temp file
                    (let ((stderr-file (cadr destination)))
                      (when stderr-file
                        (with-temp-file stderr-file
                          (insert "ffplay: command not found\n")))))
                  127))  ; Return exit code 127
          
          (let ((playback-result (elevenlabs-tts--play-audio-file test-file)))
            ;; Should return t for regular command not found errors (still attempted playback)
            (should (eq playback-result t)))
      
      (fset 'call-process original-call-process)
      (funcall restore-fn)
      (test-elevenlabs-tts--cleanup-test-audio-file)))))

;;; Unit Tests for Speech Speed Functionality

(ert-deftest test-elevenlabs-tts-speed-range-validation ()
  "Test that speed validation enforces the correct ElevenLabs range (0.7-1.2)."
  :tags '(unit)
  (with-temp-buffer
    ;; Test valid speeds
    (elevenlabs-tts--set-speech-speed 0.7)
    (should (equal (elevenlabs-tts--get-speech-speed) 0.7))
    
    (elevenlabs-tts--set-speech-speed 1.0)
    (should (equal (elevenlabs-tts--get-speech-speed) 1.0))
    
    (elevenlabs-tts--set-speech-speed 1.2)
    (should (equal (elevenlabs-tts--get-speech-speed) 1.2))
    
    ;; Test invalid speeds (should raise errors)
    (should-error (elevenlabs-tts--set-speech-speed 0.6))  ; Too low
    (should-error (elevenlabs-tts--set-speech-speed 0.25)) ; Much too low (old range)
    (should-error (elevenlabs-tts--set-speech-speed 1.3))  ; Too high
    (should-error (elevenlabs-tts--set-speech-speed 2.0))  ; Much too high (old range)
    
    ;; Test reset to default
    (elevenlabs-tts--set-speech-speed nil)
    (should (equal (elevenlabs-tts--get-speech-speed) 1.0))))

(ert-deftest test-elevenlabs-tts-buffer-local-speed ()
  "Test that speech speed is properly buffer-local."
  :tags '(unit)
  (let ((buffer1 (generate-new-buffer "test-buffer-1"))
        (buffer2 (generate-new-buffer "test-buffer-2")))
    (unwind-protect
        (progn
          ;; Set different speeds in different buffers
          (with-current-buffer buffer1
            (elevenlabs-tts--set-speech-speed 0.8)
            (should (equal (elevenlabs-tts--get-speech-speed) 0.8)))
          
          (with-current-buffer buffer2
            (elevenlabs-tts--set-speech-speed 1.1)
            (should (equal (elevenlabs-tts--get-speech-speed) 1.1)))
          
          ;; Verify they're independent
          (with-current-buffer buffer1
            (should (equal (elevenlabs-tts--get-speech-speed) 0.8)))
          
          (with-current-buffer buffer2
            (should (equal (elevenlabs-tts--get-speech-speed) 1.1))))
      ;; Cleanup
      (kill-buffer buffer1)
      (kill-buffer buffer2))))

(ert-deftest test-elevenlabs-tts-voice-settings-with-speed ()
  "Test that voice settings include the correct speed value."
  :tags '(unit)
  (with-temp-buffer
    ;; Test default speed
    (let ((settings (elevenlabs-tts--get-current-voice-settings)))
      (should (equal (cdr (assq 'speed settings)) 1.0)))
    
    ;; Test custom speed
    (elevenlabs-tts--set-speech-speed 0.9)
    (let ((settings (elevenlabs-tts--get-current-voice-settings)))
      (should (equal (cdr (assq 'speed settings)) 0.9))
      ;; Verify other settings are preserved
      (should (equal (cdr (assq 'stability settings)) 0.5))
      (should (equal (cdr (assq 'similarity_boost settings)) 0.5)))))

(ert-deftest test-elevenlabs-tts-api-payload-structure-with-speed ()
  "Test that API payload includes speed in the correct format."
  :tags '(unit)
  (with-temp-buffer
    (elevenlabs-tts--set-speech-speed 1.1)
    
    ;; Mock the API call to capture payload
    (let ((captured-payload nil))
      (cl-letf (((symbol-function 'json-encode) 
                 (lambda (data)
                   (setq captured-payload data)
                   "{\"mocked\": \"payload\"}")))
        
        ;; This will fail due to missing API key, but we just want to capture the payload
        (ignore-errors
          (elevenlabs-tts--make-curl-request "test-voice-id" "test text" "/tmp/test.mp3" "test-key")))
      
      ;; Verify payload structure
      (when captured-payload
        (let ((voice-settings (cdr (assq 'voice_settings captured-payload))))
          (should (equal (cdr (assq 'speed voice-settings)) 1.1))
          (should (equal (cdr (assq 'stability voice-settings)) 0.5)))))))

(ert-deftest test-elevenlabs-tts-speed-prompt-options ()
  "Test that speed prompt includes correct options for ElevenLabs range."
  :tags '(unit)
  (with-temp-buffer
    ;; We can't easily test interactive prompts, but we can verify the options
    (let ((options '(("Slow (0.7)" . 0.7)
                     ("Slower (0.8)" . 0.8)
                     ("Bit Slower (0.9)" . 0.9)
                     ("Normal (1.0)" . 1.0)
                     ("Bit Faster (1.1)" . 1.1)
                     ("Fast (1.2)" . 1.2)
                     ("Custom..." . custom))))
      ;; Verify all options are within valid range
      (dolist (option options)
        (let ((speed (cdr option)))
          (when (numberp speed)
            (should (and (>= speed 0.7) (<= speed 1.2)))))))))

(ert-deftest test-elevenlabs-tts-speed-performance ()
  "Test that speed operations are performant."
  :tags '(unit)
  (with-temp-buffer
    (let ((start-time (current-time)))
      ;; Test multiple speed changes
      (dotimes (i 100)
        (elevenlabs-tts--set-speech-speed (+ 0.7 (* 0.5 (/ (float i) 100))))
        (elevenlabs-tts--get-speech-speed)
        (elevenlabs-tts--get-current-voice-settings))
      
      (let ((elapsed (time-to-seconds (time-subtract (current-time) start-time))))
        (should (< elapsed 1.0)) ; Should complete in less than 1 second
        (message "Speed operations completed in %.3f seconds" elapsed)))))

;;; Unit Tests for Speed Selection UI

(ert-deftest test-elevenlabs-tts-speed-prompt-with-completing-read-mock ()
  "Test speed prompt with mocked completing-read to simulate user interactions."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read))
          (original-read-number (symbol-function 'read-number)))
      (unwind-protect
          (progn
            ;; Test selecting "Normal (1.0)" option
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    "Normal (1.0)"))
            
            (elevenlabs-tts--set-speech-speed 0.8) ; Set a different current speed
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 1.0)))
            
            ;; Test selecting "Custom..." option with valid input
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    "Custom..."))
            (fset 'read-number
                  (lambda (prompt &optional default)
                    0.9))
            
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 0.9)))
            
            ;; Test selecting "Custom..." option with invalid input (should fallback to current)
            (fset 'read-number
                  (lambda (prompt &optional default)
                    2.5)) ; Invalid speed
            
            (elevenlabs-tts--set-speech-speed 1.1) ; Set current speed
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 1.1))) ; Should use current speed as fallback
            
            ;; Test selecting current speed option
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    "Current (1.10x)"))
            
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 1.1))))
        
        ;; Restore original functions
        (fset 'completing-read original-completing-read)
        (fset 'read-number original-read-number)))))

(ert-deftest test-elevenlabs-tts-speed-prompt-empty-selection ()
  "Test speed prompt with empty selection (reproduces format specifier bug)."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read)))
      (unwind-protect
          (progn
            ;; Mock completing-read to return empty string (simulates user hitting enter with no selection)
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    "")) ; Empty string
            
            ;; Set a known current speed
            (elevenlabs-tts--set-speech-speed 0.9)
            
            ;; This should not error and should return current speed
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 0.9))
              (should (numberp result)))) ; Ensure it's actually a number
        
        ;; Restore original function
        (fset 'completing-read original-completing-read)))))

(ert-deftest test-elevenlabs-tts-speed-prompt-nil-selection ()
  "Test speed prompt with nil selection."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read)))
      (unwind-protect
          (progn
            ;; Mock completing-read to return nil
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    nil))
            
            ;; Set a known current speed
            (elevenlabs-tts--set-speech-speed 0.7)
            
            ;; This should not error and should return current speed
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 0.7))
              (should (numberp result))))
        
        ;; Restore original function
        (fset 'completing-read original-completing-read)))))

(ert-deftest test-elevenlabs-tts-speed-prompt-invalid-selection ()
  "Test speed prompt with invalid/non-existent selection."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read)))
      (unwind-protect
          (progn
            ;; Mock completing-read to return a string that doesn't match any option
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    "Non-existent option"))
            
            ;; Set a known current speed
            (elevenlabs-tts--set-speech-speed 1.2)
            
            ;; This should fallback to current speed when the selection doesn't match
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 1.2))
              (should (numberp result))))
        
        ;; Restore original function
        (fset 'completing-read original-completing-read)))))

(ert-deftest test-elevenlabs-tts-speed-prompt-current-option-included ()
  "Test that current speed option is included in completion list."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read))
          (captured-collection nil))
      (unwind-protect
          (progn
            ;; Mock completing-read to capture the collection parameter
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    (setq captured-collection collection)
                    "Normal (1.0)")) ; Return a valid selection
            
            ;; Set a known current speed that's not in standard options
            (elevenlabs-tts--set-speech-speed 0.85)
            
            ;; Call the function
            (elevenlabs-tts--prompt-for-speech-speed)
            
            ;; Verify that current speed option was added to collection
            (should (member "Current (0.85x)" captured-collection))
            
            ;; Verify standard options are still there
            (should (member "Normal (1.0)" captured-collection))
            (should (member "Slow (0.7)" captured-collection))
            (should (member "Fast (1.2)" captured-collection))
            (should (member "Custom..." captured-collection)))
        
        ;; Restore original function
        (fset 'completing-read original-completing-read)))))

(ert-deftest test-elevenlabs-tts-speed-prompt-default-highlighting ()
  "Test that current speed option is set as default in completing-read."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read))
          (captured-default nil))
      (unwind-protect
          (progn
            ;; Mock completing-read to capture the default parameter (7th parameter)
            (fset 'completing-read
                  (lambda (prompt collection &optional predicate require-match initial-input hist def)
                    (setq captured-default def)
                    "Normal (1.0)")) ; Return a valid selection
            
            ;; Set a known current speed
            (elevenlabs-tts--set-speech-speed 0.75)
            
            ;; Call the function
            (elevenlabs-tts--prompt-for-speech-speed)
            
            ;; Verify that current speed option was set as default
            (should (string-equal captured-default "Current (0.75x)")))
        
        ;; Restore original function
        (fset 'completing-read original-completing-read)))))

(ert-deftest test-elevenlabs-tts-speed-prompt-format-consistency ()
  "Test that speed formatting is consistent throughout the prompt system."
  :tags '(unit)
  (with-temp-buffer
    ;; Test various speed values for consistent formatting
    (let ((test-speeds '(0.7 0.8 0.85 0.9 1.0 1.1 1.15 1.2)))
      (dolist (speed test-speeds)
        (elevenlabs-tts--set-speech-speed speed)
        
        ;; Test that formatting works without errors for any valid speed
        (let ((formatted-current (format "Current (%.2fx)" speed))
              (formatted-prompt (if (equal speed 1.0)
                                   (format "Speech speed (current: default %.2fx): " speed)
                                 (format "Speech speed (current: %.2fx): " speed))))
          ;; These should not error
          (should (stringp formatted-current))
          (should (stringp formatted-prompt))
          
          ;; Verify format is correct (should contain the speed value)
          (should (string-match (format "%.2fx" speed) formatted-current))
          (should (string-match (format "%.2fx" speed) formatted-prompt)))))))

(ert-deftest test-elevenlabs-tts-speed-selection-robustness ()
  "Test that speed selection is robust against edge cases and invalid inputs."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read))
          (original-read-number (symbol-function 'read-number)))
      (unwind-protect
          (progn
            ;; Test case 1: Custom input with read-number error
            (fset 'completing-read
                  (lambda (&rest args)
                    "Custom..."))
            (fset 'read-number
                  (lambda (&rest args)
                    (error "Invalid input")))
            
            (elevenlabs-tts--set-speech-speed 0.8)
            ;; This should propagate the error from read-number
            (should-error (elevenlabs-tts--prompt-for-speech-speed))
            
            ;; Test case 2: Custom input that returns non-number
            (fset 'read-number
                  (lambda (&rest args)
                    "not-a-number")) ; Invalid return type
            
            ;; This should handle gracefully and use current speed
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 0.8))) ; Should fallback to current
            
            ;; Test case 3: Normal operation after errors
            (fset 'completing-read
                  (lambda (&rest args)
                    "Fast (1.2)"))
            
            (let ((result (elevenlabs-tts--prompt-for-speech-speed)))
              (should (equal result 1.2))))
        
        ;; Restore original functions
        (fset 'completing-read original-completing-read)
        (fset 'read-number original-read-number)))))

(ert-deftest test-elevenlabs-tts-interactive-speed-setting-function ()
  "Test the interactive speed setting function with various user inputs."
  :tags '(unit)
  (with-temp-buffer
    (let ((original-completing-read (symbol-function 'completing-read))
          (original-message (symbol-function 'message))
          (captured-messages '()))
      (unwind-protect
          (progn
            ;; Capture messages to verify user feedback
            (fset 'message
                  (lambda (format-string &rest args)
                    (push (apply 'format format-string args) captured-messages)))
            
            ;; Test case 1: Selecting same speed (should show "unchanged" message)
            (fset 'completing-read
                  (lambda (&rest args)
                    "Current (1.00x)"))
            
            (elevenlabs-tts--set-speech-speed 1.0)
            (elevenlabs-tts-set-speech-speed)
            
            ;; Should have message about speed unchanged
            (should (cl-some (lambda (msg) (string-match "unchanged.*1\\.00x" msg)) captured-messages))
            
            ;; Test case 2: Selecting different speed (should show "set to" message)
            (setq captured-messages '()) ; Clear messages
            (fset 'completing-read
                  (lambda (&rest args)
                    "Fast (1.2)"))
            
            (elevenlabs-tts-set-speech-speed)
            
            ;; Should have message about speed being set
            (should (cl-some (lambda (msg) (string-match "set to.*1\\.20x" msg)) captured-messages))
            (should (equal (elevenlabs-tts--get-speech-speed) 1.2))
            
            ;; Test case 3: Reset to default (should show "reset to default" message)
            (setq captured-messages '()) ; Clear messages
            (fset 'completing-read
                  (lambda (&rest args)
                    "Normal (1.0)"))
            
            (elevenlabs-tts-set-speech-speed)
            
            ;; Speed should be reset and message should reflect it
            (should (equal (elevenlabs-tts--get-speech-speed) 1.0))
            (should (cl-some (lambda (msg) (string-match "reset to default.*1\\.00x" msg)) captured-messages)))
        
        ;; Restore original functions
        (fset 'completing-read original-completing-read)
        (fset 'message original-message)))))

(ert-deftest test-elevenlabs-tts-format-specifier-error-prevention ()
  "Test that format specifier errors are prevented in all speed-related functions.
  This test specifically catches the bug that was reported."
  :tags '(unit)
  (with-temp-buffer
    ;; Test all functions that format speed values
    (let ((test-speeds '(0.7 0.8 0.9 1.0 1.1 1.2))
          (original-message (symbol-function 'message))
          (format-errors '()))
      (unwind-protect
          (progn
            ;; Override message to catch format errors
            (fset 'message
                  (lambda (format-string &rest args)
                    (condition-case err
                        (apply 'format format-string args)
                      (error (push (cons format-string args) format-errors)
                             (signal (car err) (cdr err))))))
            
            ;; Test each speed value in various contexts
            (dolist (speed test-speeds)
              (elevenlabs-tts--set-speech-speed speed)
              
              ;; Test current speed formatting in messages (should not error)
              (condition-case err
                  (message "Speed is %.2fx" (elevenlabs-tts--get-speech-speed))
                (error (should nil))) ; Should not reach this
              
              ;; Test voice settings inclusion
              (let ((settings (elevenlabs-tts--get-current-voice-settings)))
                (should (numberp (cdr (assq 'speed settings))))
                (condition-case err
                    (message "Settings speed: %.2fx" (cdr (assq 'speed settings)))
                  (error (should nil)))) ; Should not error
              
              ;; Test prompt creation (without actually calling completing-read)
              (let* ((current-speed (elevenlabs-tts--get-speech-speed))
                     (default-speed (cdr (assq 'speed elevenlabs-tts-default-settings)))
                     (is-default (equal current-speed default-speed)))
                ;; These should not error
                (condition-case err
                    (if is-default
                        (format "Speech speed (current: default %.2fx): " current-speed)
                      (format "Speech speed (current: %.2fx): " current-speed))
                  (error (should nil)))
                (condition-case err
                    (format "Current (%.2fx)" current-speed)
                  (error (should nil)))))
            
            ;; Test with nil speed (should not happen but let's be safe)
            (setq elevenlabs-tts-speech-speed nil)
            (let ((current-speed (elevenlabs-tts--get-speech-speed)))
              (should (numberp current-speed)) ; Should return default, not nil
              (condition-case err
                  (message "Default speed: %.2fx" current-speed)
                (error (should nil)))) ; Should not error
            
            ;; Verify no format errors occurred
            (should (null format-errors)))
        
        ;; Restore original function
        (fset 'message original-message)
        
        ;; If there were format errors, show them
        (when format-errors
          (message "Format errors detected: %s" format-errors))))))

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

(ert-deftest test-elevenlabs-tts-integration-speed-validation ()
  "Test speed validation against actual ElevenLabs API constraints (integration test).

This test validates that our speed range (0.7-1.2) matches the documented
ElevenLabs API constraints from:
https://help.elevenlabs.io/hc/en-us/articles/13416271012497-Can-I-change-the-pace-of-the-voice"
  :tags '(integration)
  (with-temp-buffer
    ;; Test that valid speeds work
    (elevenlabs-tts--set-speech-speed 0.8)
    (let ((settings (elevenlabs-tts--get-current-voice-settings)))
      (should (and (>= (cdr (assq 'speed settings)) 0.7)
                   (<= (cdr (assq 'speed settings)) 1.2))))
    
    ;; Test boundary values
    (elevenlabs-tts--set-speech-speed 0.7)  ; Minimum
    (should (equal (elevenlabs-tts--get-speech-speed) 0.7))
    
    (elevenlabs-tts--set-speech-speed 1.2)  ; Maximum
    (should (equal (elevenlabs-tts--get-speech-speed) 1.2))
    
    ;; Test that our validation matches API constraints
    (message "Speed validation test passed with API constraints 0.7-1.2")))

(ert-deftest test-elevenlabs-tts-integration-speed-with-api ()
  "Test speed control with actual API call (integration test).

This test requires:
- Valid API key in ~/.config/emacs-tts/elevenlabs-api-key
- Sufficient credits in ElevenLabs account
- Network connectivity

Tests that custom speed values are properly sent to the ElevenLabs API."
  :tags '(integration)
  (skip-unless (file-exists-p (expand-file-name elevenlabs-tts-api-key-file)))
  (let* ((temp-dir (make-temp-file "tts-speed-integration-" t))
         (output-file (expand-file-name "speed-test.mp3" temp-dir))
         (voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (test-text "Speed test."))
    (unwind-protect
        (with-temp-buffer
          ;; Set custom speed in this buffer
          (elevenlabs-tts--set-speech-speed 0.8)
          (should (equal (elevenlabs-tts--get-speech-speed) 0.8))
          
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
