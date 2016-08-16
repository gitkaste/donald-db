(defun oed-lookup (string)
  (setenv "OED_LOOKUP" (format "%s" string))
  (shell-command-to-string "~/bin/oed.pl"))

(defun fs-oed (word) (oed-lookup word))
