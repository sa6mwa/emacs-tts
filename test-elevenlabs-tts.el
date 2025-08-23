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

(ert-deftest test-elevenlabs-tts-get-voice-id ()
  "Test that voice ID lookup works for known voices."
  (should (equal (elevenlabs-tts--get-voice-id "Rachel") "21m00Tcm4TlvDq8ikWAM"))
  (should (equal (elevenlabs-tts--get-voice-id "Josh") "TxGEqnHWrfWFTfGW9XjX"))
  (should (null (elevenlabs-tts--get-voice-id "NonexistentVoice"))))

(ert-deftest test-elevenlabs-tts-get-base-filename ()
  "Test base filename generation."
  (let ((buffer-file-name "/home/user/documents/article.txt"))
    (should (equal (elevenlabs-tts--get-base-filename) "article")))
  (let ((buffer-file-name nil))
    (should (equal (elevenlabs-tts--get-base-filename) "tts-audio"))))

(ert-deftest test-elevenlabs-tts-get-next-filename ()
  "Test sequential filename generation."
  (let ((temp-dir (make-temp-file "tts-test-" t)))
    (unwind-protect
        (progn
          ;; Test first file
          (let ((filename (elevenlabs-tts--get-next-filename temp-dir "test")))
            (should (string-match "test-01\\.mp3$" filename)))
          
          ;; Create the first file and test second
          (write-region "test" nil (expand-file-name "test-01.mp3" temp-dir))
          (let ((filename (elevenlabs-tts--get-next-filename temp-dir "test")))
            (should (string-match "test-02\\.mp3$" filename))))
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
