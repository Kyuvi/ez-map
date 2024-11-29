;;; ez-map.el --- Making keybinding eazy  -*- lexical-binding: t; -*-
;;
;; Copyright © 2024 by Kyuvi (MIT license)
;;;
;;;Author: Kyuvi <kyuvi@gmx.com>
;;;Version: 0.01
;;;Package requires: ((general) (cl-lib))
;;;Keywords: keybinding bindings keys evil doom extensions convenience
;;;
;;;  Description:
;;
;; A centralized keybinds system, which can integrate with `which-key' to preview
;; available keybindings. Centered on the powerful macro: `map!' and including the helper
;; macros `cmd!', `cmd!!', `cmds!', `kbd!' and `after!'.
;; If evil is never loaded, then evil bindings set with `map!' are ignored
;; (i.e. omitted entirely for performance reasons).
;;
;;; Commentary:
;;
;;  Ez-map is an implimentation of Doom Emacs' `map!' macro to make mapping
;;  keybindings easier in Emacs. It depends on `general.el' (and `cl-lib').
;;
;;   I took the neccesary `doom-lib.el' and `doom-keybinds.el' functions and
;;   merged them into `ez-map.el' removing the dependencies on `use-package',
;;   `which-key' (which now needs to be configured seperately if used) and the rest
;;   of the doom ecosystem.
;;   .
;;;
;;; Code:


(require 'general)
(require 'cl-lib)

;; helper functions

(defun ez-map-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun ez-map-keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defun ez-map-ensure-list (obj)
  "Return OBJECT as a list.

If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
  (if (listp obj) obj (list obj)))

;; mutation
(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

;; cmd! macros

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))


(defmacro cmd!! (command &optional prefix-arg &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG.
Like `cmd!', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `map!')."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))

(defmacro cmds! (&rest branches)
  "Returns a dispatcher that runs the a command in BRANCHES.
Meant to be used as a target for keybinds (e.g. with `define-key' or `map!').

BRANCHES is a flat list of CONDITION COMMAND pairs. CONDITION is a lisp form
that is evaluated when (and each time) the dispatcher is invoked. If it returns
non-nil, COMMAND is invoked, otherwise it falls through to the next pair.

The last element of BRANCHES can be a COMMAND with no CONDITION. This acts as
the fallback if all other conditions fail.

Otherwise, Emacs will fall through the keybind and search the next keymap for a
keybind (as if this keybind never existed).

See `general-key-dispatch' for what other arguments it accepts in BRANCHES."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by 'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,(or docstring "") nil
         :filter (lambda (&optional _)
                   (let (it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback))))))))

;; after!

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and diff-hl have loaded)
    (after! (magit diff-hl) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' but supports compound package statements
(see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))





                    ;;;;;;;; Keybindings ;;;;;;;;

(defalias 'kbd! #'general-simulate-key)

;; initial leader keys

(defvar ez-map-leader-key "SPC"
  "The leader prefix key for Evil users.")

(defvar ez-map-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

(defvar ez-map-leader-key-states '(normal visual motion)
  "which evil modes to activate the leader key for")


(defvar ez-map-leader-alt-key-states '(emacs insert)
  "which evil modes to activate the alternative leader key for")

(defvar ez-map-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(defvar ez-map-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.")

(defvar ez-map-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")   

;;; Global keybind settings

(cond
 ((eq system-type 'darwin) ;doom--system-macos-p
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 ((memq system-type '(cygwin windows-nt ms-dos)) ;doom--system-windows-p
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;; HACK: Emacs can't distinguish C-i from TAB, or C-m from RET, in either GUI or
;;   TTY frames.  This is a byproduct of its history with the terminal, which
;;   can't distinguish them either, however, Emacs has separate input events for
;;   many contentious keys like TAB and RET (like [tab] and [return], aka
;;   "<tab>" and "<return>"), which are only triggered in GUI frames, so here, I
;;   create one for C-i. Won't work in TTY frames, though. ezv's :os tty module
;;   has a workaround for that though.
(pcase-dolist (`(,key ,fallback . ,events)
               '(([C-i] [?\C-i] tab kp-tab)
                 ([C-m] [?\C-m] return kp-return)))
  (define-key
   input-decode-map fallback
   (cmd! (if (when-let ((keys (this-single-command-raw-keys)))
               (and (display-graphic-p)
                    (not (cl-loop for event in events
                                  if (cl-position event keys)
                                  return t))
                    ;; Use FALLBACK if nothing is bound to KEY, otherwise we've
                    ;; broken all pre-existing FALLBACK keybinds.
                    (key-binding
                     (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                              key) nil t)))
             key fallback))))


;;
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. ESC/C-g should
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; one press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.
;;
(defvar ez-map-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).

More specifically, when `ez-map/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun ez-map/escape (&optional interactive)
  "Run `ez-map-escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success 'ez-map-escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))


(global-set-key [remap keyboard-quit] #'ez-map/escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command 'ez-map/escape))

(after! evil
;;;###autoload
  (defun +evil-escape-a (&rest _)
    "Call `ez-map/escape' if `evil-force-normal-state' is called interactively."
    (when (called-interactively-p 'any)
      (call-interactively #'ez-map/escape)))

  (advice-add #'evil-force-normal-state :after #'+evil-escape-a))

;;
;;; General + leader/localleader keys

;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'undefine-key! #'general-unbind)
;; Prevent "X starts with non-prefix key Y" errors except at startup.
(add-hook 'ez-map-after-modules-init-hook #'general-auto-unbind-keys)

;; HACK: `map!' uses this instead of `define-leader-key!' because it consumes
;;   20-30% more startup time, so we reimplement it ourselves.
(defmacro ez-map--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
            (def (pop keys)))
        (if (keywordp key)
            (when (memq key '(:prefix :infix))
              (setq prefix def))
          (when prefix
            (setq key `(general--concat t ,prefix ,key)))
          (let* ((udef (cdr-safe (ez-map-unquote def)))
                 (bdef (if (general--extended-def-p udef)
                           (general--extract-def
                            (general--normalize-extended-def udef))
                         def)))
            (unless (eq bdef :ignore)
              (push `(define-key ez-map-leader-map (general--kbd ,key)
                       ,bdef)
                    forms))
            (when-let (desc (cadr (memq :which-key udef)))
              (prependq!
               wkforms `((which-key-add-key-based-replacements
                           (general--concat t ez-map-leader-alt-key ,key)
                           ,desc)
                         (which-key-add-key-based-replacements
                           (general--concat t ez-map-leader-key ,key)
                           ,desc))))))))
    (macroexp-progn
     (append (and wkforms `((after! which-key ,@(nreverse wkforms))))
             (nreverse forms)))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.

Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.

See `ez-map-leader-key' and `ez-map-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'ez-map-leader-map
    ,@args))


(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `ez-map-localleader-key' and `ez-map-localleader-alt-key' to change the
localleader prefix."
  ;; not quite sure which correctly models modulep!
  (if (featurep 'evil) ;(modulep! :editor evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
        :states '(normal visual motion emacs insert)
        :major-modes t
        :prefix ez-map-localleader-key
        :non-normal-prefix ez-map-localleader-alt-key
        ,@args)
    `(general-define-key
      :major-modes t
      :prefix ez-map-localleader-alt-key
      ,@args)))



;; PERF: We use a prefix commands instead of general's
;;   :prefix/:non-normal-prefix properties because general is incredibly slow
;;   binding keys en mass with them in conjunction with :states -- an effective
;;   doubling of ez-map's startup time!
(define-prefix-command 'ez-map/leader 'ez-map-leader-map)
(define-key ez-map-leader-map [override-state] 'all) ;; TODO: why/what is this?

;; Bind `ez-map-leader-key' and `ez-map-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(add-hook 'ez-map-after-init-hook
  (defun ez-map-init-leader-keys ()
    "Bind `ez-map-leader-key' and `ez-map-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
          (progn
            (cond ((equal ez-map-leader-alt-key "C-c")
                   (set-keymap-parent ez-map-leader-map mode-specific-map))
                  ((equal ez-map-leader-alt-key "C-x")
                   (set-keymap-parent ez-map-leader-map ctl-x-map)))
            (define-key map (kbd ez-map-leader-alt-key) 'ez-map/leader))
        (evil-define-key*
          ez-map-leader-key-states map (kbd ez-map-leader-key) 'ez-map/leader)
        (evil-define-key*
          ez-map-leader-alt-key-states map (kbd ez-map-leader-alt-key) 'ez-map/leader))
      (general-override-mode +1))))


;; which-key initialization

(after! which-key
    (which-key-add-key-based-replacements ez-map-leader-key "<leader>")
    (which-key-add-key-based-replacements ez-map-localleader-key "<localleader>"))


;;
;;; `map!' macro

(defvar ez-map-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")


(defun ez-map--map-keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`ez-map-evil-state-alist' to customize this."
  (cl-loop for l across (ez-map-keyword-name keyword)
           if (assq l ez-map-evil-state-alist) collect (cdr it)
           else do (error "not a valid state: %s" l)))



;; specials
(defvar ez-map--map-forms nil)
(defvar ez-map--map-fn nil)
(defvar ez-map--map-batch-forms nil)
(defvar ez-map--map-state '(:dummy t))
(defvar ez-map--map-parent-state nil)
(defvar ez-map--map-evil-p nil)
(after! evil (setq ez-map--map-evil-p t))

(defun ez-map--map-process (rest)
  (let ((ez-map--map-fn ez-map--map-fn)
        ez-map--map-state
        ez-map--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (ez-map--map-nested nil key))

              ((keywordp key)
               (pcase key
                 (:leader
                  (ez-map--map-commit)
                  (setq ez-map--map-fn 'ez-map--define-leader-key))
                 (:localleader
                  (ez-map--map-commit)
                  (setq ez-map--map-fn 'define-localleader-key!))
                 (:after
                  (ez-map--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (ez-map--map-set :keymaps
                                  `(backquote ,(ez-map-ensure-list (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (ez-map-ensure-list (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (ez-map--map-nested (list (intern (ez-map-keyword-name key))
                                           (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (let ((keymap (intern (format "ez-map-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            ez-map--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (let ((arg (pop rest)))
                        (if (consp arg) arg (list arg)))
                    (ez-map--map-set (if ez-map--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          ez-map--map-forms)))
                 (_
                  (condition-case _
                      (ez-map--map-def (pop rest) (pop rest)
                                     (ez-map--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((ez-map--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (ez-map--map-commit)
    (macroexp-progn (nreverse (delq nil ez-map--map-forms)))))



(defun ez-map--map-append-keys (prop)
  (let ((a (plist-get ez-map--map-parent-state prop))
        (b (plist-get ez-map--map-state prop)))
    (if (and a b)
        `(general--concat t ,a ,b)
      (or a b))))


(defun ez-map--map-nested (wrapper rest)
  (ez-map--map-commit)
  (let ((ez-map--map-parent-state (ez-map--map-state)))
    (push (if wrapper
              (append wrapper (list (ez-map--map-process rest)))
            (ez-map--map-process rest))
          ez-map--map-forms)))

(defun ez-map--map-set (prop &optional value)
  (unless (equal (plist-get ez-map--map-state prop) value)
    (ez-map--map-commit))
  (setq ez-map--map-state (plist-put ez-map--map-state prop value)))

(defun ez-map--map-def (key def &optional states desc)
  (when (or (memq 'global states)
            (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
                  (keywordp (car-safe (setq unquoted (ez-map-unquote def)))))
             (setq def (list 'quote (plist-put unquoted :which-key desc))))
            ((setq def (cons 'list
                             (if (and (equal key "")
                                      (null def))
                                 `(:ignore t :which-key ,desc)
                               (plist-put (general--normalize-extended-def def)
                                          :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
          (alist-get state ez-map--map-batch-forms)))
  t)

(defun ez-map--map-commit ()
  (when ez-map--map-batch-forms
    (cl-loop with attrs = (ez-map--map-state)
             for (state . defs) in ez-map--map-batch-forms
             if (or ez-map--map-evil-p (not state))
             collect `(,(or ez-map--map-fn 'general-define-key)
                       ,@(if state `(:states ',state)) ,@attrs
                       ,@(cl-mapcan #'identity (nreverse defs)))
             into forms
             finally do (push (macroexp-progn forms) ez-map--map-forms))
    (setq ez-map--map-batch-forms nil)))

(defun ez-map--map-state ()
  (let ((plist
         (append (list :prefix (ez-map--map-append-keys :prefix)
                       :infix  (ez-map--map-append-keys :infix)
                       :keymaps
                       (append (plist-get ez-map--map-parent-state :keymaps)
                               (plist-get ez-map--map-state :keymaps)))
                 ez-map--map-state
                 nil))
        newplist)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when (and val (not (plist-member newplist key)))
          (push val newplist)
          (push key newplist))))
    newplist))

(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

Properties
  :leader [...]                   an alias for (:prefix ez-map-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
                                  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
                                  where the following keys will be bound. DO NOT
                                  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

Key-string descriptions
  :desc DESCRIPTION :State(s) key-string function

  The :desc keyword depends on `which-key' being loaded.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

  These must be placed right before the key string.

  Do
    (map! :leader :desc \"Description\" :n \"C-c\" #'dosomething)
  Don't
    (map! :n :leader :desc \"Description\" \"C-c\" #'dosomething)
    (map! :leader :n :desc \"Description\" \"C-c\" #'dosomething)"
  (when (or (bound-and-true-p byte-compile-current-file)
            (not noninteractive))
    (ez-map--map-process rest)))


(provide 'ez-map)
