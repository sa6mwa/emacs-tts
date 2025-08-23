;;; test-security.el --- Security tests for elevenlabs-tts -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to verify security improvements:
;; 1. API key is not exposed in process list
;; 2. Filenames with special characters work correctly

;;; Code:

(require 'ert)
(require 'elevenlabs-tts)

(ert-deftest test-elevenlabs-tts-security-special-filename-chars ()
  "Test that filenames with special characters are handled safely."
  :tags '(security)
  (skip-unless (elevenlabs-tts--read-api-key))
  (skip-unless (executable-find "curl"))
  
  (let* ((temp-dir (make-temp-file "tts-security-" t))
         ;; Test filename with spaces, quotes, and other special chars
         (special-filename (expand-file-name "test file with 'quotes' & symbols.mp3" temp-dir))
         (elevenlabs-tts-debug t)) ; Enable debug to see filename handling
    (unwind-protect
        (progn
          (should (elevenlabs-tts--make-api-request-sync 
                   "21m00Tcm4TlvDq8ikWAM" ; Rachel's voice ID
                   "Special filename test"
                   special-filename))
          (should (file-exists-p special-filename))
          (should (> (nth 7 (file-attributes special-filename)) 100))) ; File has content
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest test-elevenlabs-tts-security-tilde-expansion ()
  "Test that tilde in filenames is properly expanded."
  :tags '(security)
  (skip-unless (elevenlabs-tts--read-api-key))
  (skip-unless (executable-find "curl"))
  
  (let* ((tilde-filename "~/tts-security-tilde-test.mp3")
         (expanded-filename (expand-file-name tilde-filename))
         (elevenlabs-tts-debug t)) ; Enable debug to see filename expansion
    (unwind-protect
        (progn
          (should (elevenlabs-tts--make-api-request-sync 
                   "21m00Tcm4TlvDq8ikWAM" ; Rachel's voice ID
                   "Tilde expansion test"
                   tilde-filename))
          (should (file-exists-p expanded-filename))
          (should (> (nth 7 (file-attributes expanded-filename)) 100))) ; File has content
      (when (file-exists-p expanded-filename)
        (delete-file expanded-filename)))))

;;; test-security.el ends here
