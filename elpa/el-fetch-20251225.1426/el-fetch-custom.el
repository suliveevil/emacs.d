;;; el-fetch-custom.el --- El-Fetch customization -*- lexical-binding: t -*-

;; This file is part of xgqt-elisp-app-el-fetch - sys info, Neofetch-style in Emacs.
;; Copyright (c) 2022-2025, Maciej Barć <xgqt@riseup.net>
;; Licensed under the GNU GPL v2 License
;;
;; xgqt-elisp-app-el-fetch is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; xgqt-elisp-app-el-fetch is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with xgqt-elisp-app-el-fetch.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; El-Fetch customization.

;;; Code:

(defgroup el-fetch nil
  "System information in Neofetch-like style."
  :group 'applications)

(defmacro def-el-fetch-custom-information-toggle (symbol-identifier information-part-comment)
  "Quick defcustom for El-Fetch information parts.
SYMBOL-IDENTIFIER is the variable to bind new defcustom.
INFORMATION-PART-COMMENT is used for defcustom docstring."
  `(defcustom ,symbol-identifier t
     ,(concat "Show information related to the " information-part-comment ".")
     :type 'boolean
     :group 'el-fetch))

(def-el-fetch-custom-information-toggle el-fetch-custom-info-cpu "CPU")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-memory "memory (RAM)")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-device "device model")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-distro "system distribution")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-kernel "system kernel")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-shell "SHELL")

(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-version "Emacs version")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-user-dir "Emacs user directory")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-pkgs "Emacs installed packages")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-load-path "Emacs load path")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-font "Emacs font")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-theme "Emacs theme")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-bars "Emacs bars")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-frame "Emacs frame")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-completion "Emacs completion framework")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-buffers "Emacs buffers")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-processes "Emacs processes")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-emacs-uptime "Emacs uptime")
(def-el-fetch-custom-information-toggle el-fetch-custom-info-group-memory-use "Emacs internal memory use")

(provide 'el-fetch-custom)

;;; el-fetch-custom.el ends here
