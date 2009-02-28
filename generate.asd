;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; -*-

(defpackage #:generate-system
  (:use #:cl #:asdf))

(in-package #:generate-system)

(asdf:defsystem #:generate
  :name "generate"
  :author "Olof-Joachim Frahm <Olof.Frahm@web.de>"
  :maintainer "Olof-Joachim Frahm <Olof.Frahm@web.de>"
  :licence "GPL3"
  :description "A simple static blog renderer"
  :serial T
  :depends-on (:split-sequence
	       :parenscript
	       :anaphora
	       :cl-who
	       :puri
	       :css-lite
	       :cl-markdown
	       :local-time
	       :logv
	       :cl-ppcre)
  :components ((:file "defpackage")
	       (:file "generate")))
