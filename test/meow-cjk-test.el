;;; meow-cjk-test.el --- ERT tests for meow-cjk  -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'meow-cjk)

(ert-deftest meow-cjk-test-mode-enable-does-not-require-emt-module ()
  (let ((emt-lib-path (make-temp-name "emt-missing-module-"))
        (emt--lib-loaded nil))
    (unwind-protect
        (progn
          (meow-cjk-mode 1)
          (should meow-cjk-mode))
      (meow-cjk-mode 0))))

(provide 'meow-cjk-test)

;;; meow-cjk-test.el ends here
