;;; osm-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))



;;; Generated autoloads from osm.el

(autoload 'osm-home "osm" "\
Go to home coordinates." t)
(autoload 'osm-goto "osm" "\
Go to LAT/LON/ZOOM.

(fn LAT LON ZOOM)" t)
(autoload 'osm "osm" "\
Go to LINK.

(fn &rest LINK)" nil t)
(autoload 'osm-bookmark-jump "osm" "\
Jump to osm bookmark BM.

(fn BM)" t)
(autoload 'osm-bookmark-delete "osm" "\
Delete osm bookmark BM.

(fn BM)" t)
(autoload 'osm-bookmark-rename "osm" "\
Rename osm bookmark OLD-NAME.

(fn OLD-NAME)" t)
(autoload 'osm-search "osm" "\
Search for SEARCH and display the map.
If the prefix argument LUCKY is non-nil take the first result and jump there.

(fn SEARCH &optional LUCKY)" t)
(autoload 'osm-gpx-show "osm" "\
Show the tracks of gpx FILE in an `osm-mode' buffer.

(fn FILE)" t)
(autoload 'osm-server "osm" "\
Select tile SERVER.

(fn SERVER)" t)
(register-definition-prefixes "osm" '("osm-"))


;;; Generated autoloads from osm-ol.el

(register-definition-prefixes "osm-ol" '("osm-ol-"))

;;; End of scraped data

(provide 'osm-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; osm-autoloads.el ends here
