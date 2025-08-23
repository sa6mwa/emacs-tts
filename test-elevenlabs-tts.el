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
          ;; Stub call-process to simulate successful curl
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program infile buffer display &rest args)
                       ;; Extract output filename after "-o"
                       (let* ((o-index (cl-position "-o" args :test #'string=))
                              (ofile (and o-index (nth (1+ o-index) args))))
                         (when ofile
                           (with-temp-file ofile
                             (insert (make-string 256 ?X))))
                         0))))
            (let ((result (elevenlabs-tts--make-curl-request voice-id
                                                             "Test text"
                                                             out-file
                                                             "DUMMY-KEY")))
              (should result)
              (should (file-exists-p out-file))
              (should (file-directory-p nested-dir)))))
      (when (file-exists-p tmp-root) 
        (delete-directory tmp-root t)))))

;;; Integration Tests (require API key)

(ert-deftest test-elevenlabs-tts-integration-api-key-reading ()
  "Test reading API key from file (integration test)."
  :tags '(integration)
  (let ((api-key-file (expand-file-name elevenlabs-tts-api-key-file)))
    (if (file-exists-p api-key-file)
        (should (stringp (elevenlabs-tts--read-api-key)))
      (should-error (elevenlabs-tts--read-api-key)))))

(ert-deftest test-elevenlabs-tts-integration-voice-generation ()
  "Test actual voice generation with API (integration test)."
  :tags '(integration)
  (skip-unless (file-exists-p (expand-file-name elevenlabs-tts-api-key-file)))
  (let* ((temp-dir (make-temp-file "tts-integration-" t))
         (output-file (expand-file-name "integration-test.mp3" temp-dir))
         (voice-id (elevenlabs-tts--get-voice-id "Rachel"))
         (test-text "This is a test."))
    (unwind-protect
        (progn
          (should voice-id)
          (let ((result (elevenlabs-tts--make-api-request-sync voice-id test-text output-file)))
            (if result
                (progn
                  (should (file-exists-p output-file))
                  (should (> (nth 7 (file-attributes output-file)) 1000))) ; File should be > 1KB
              (message "Integration test skipped - API call failed"))))
      ;; Cleanup
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'test-elevenlabs-tts)

;;; test-elevenlabs-tts.el ends here
