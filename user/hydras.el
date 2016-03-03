;; modal navigation

(defhydra hydra-navigate (:color red
				 :hint nil)
  "
_f_: forward-char       _w_: forward-word       _n_: next-line
_b_: backward-char      _W_: backward-word      _p_: previous-line
^ ^                     _o_: subword-right      _,_: beginning-of-line
^ ^                     _O_: subword-left       _._: end-of-line

_s_: forward sentence   _a_: forward paragraph  _e_: forward page
_S_: backward sentence  _A_: backward paragraph _E_: backward page

_h_: helm mini _u_: buffer list _i_: window  _g_: goto  _d_: delete
_<left>_: previous buffer   _<right>_: next buffer
_<up>_: scroll-up           _<down>_: scroll-down

_[_: backward-sexp _]_: forward-sexp
_<_ beginning of buffer _>_ end of buffer _m_: set mark _/_: jump to mark
"
  ("f" forward-char)
  ("b" backward-char)
  ("w" forward-word)
  ("W" backward-word)
  ("n" next-line)
  ("p" previous-line)
  ("o" subword-right)
  ("O" subword-left)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("a" forward-paragraph)
  ("A" backward-paragraph)
  ("e" forward-page)
  ("E" backward-page)
  ("g" goto/body :color blue)
  ("<right>" next-buffer)
  ("<left>" previous-buffer)
  ("h" helm-mini :color blue)
  ("i" ace-window :color blue)
  ("m" org-mark-ring-push)
  ("/" org-mark-ring-goto :color blue)
  ("u" helm-buffers-list)
  ("<up>" scroll-up)
  ("<down>" scroll-down)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("." end-of-line)
  ("[" backward-sexp)
  ("]" forward-sexp)
  ("," beginning-of-line)
  ("d" hydra-delete/body :color blue)
  ("q" nil "quit" :color blue))

(defun org-goto-target (target)
  (interactive (list  (ido-completing-read  "Target: " (org-all-targets))))
  (org-open-link-from-string (format "[[%s]]" target)))



(global-set-key (kbd "s-n") 'hydra-navigate/body)

;; * modal goto
(defhydra hydra-goto-word (:color blue :hint nil)
  "
_j_: word-0 _k_: word-1 _l_: subword-0 _;_: subword-1"

  ("j" avy-goto-word-1)
  ("k" avy-goto-word-0)
  ("l" avy-goto-subword-1)
  (";" avy-goto-subword-0))


(defhydra hydra-goto-char (:color blue :hint nil)
  "
_f_: char timer _j_: char-2 _k_: char _l_: char inline"

  ("f" avy-goto-char-timer)
  ("j" avy-goto-char-2)
  ("k" avy-goto-char)
  ("l" avy-goto-char-in-line))


(defhydra hydra-goto-line (:color blue :hint nil)
  "
_j_: line above _k_: line _l_: line below"

  ("j" avy-goto-line-above)
  ("k" avy-goto-line)
  ("l" avy-goto-line-below))


(defhydra goto (:color blue :hint nil)
  "
Goto:
^item^              ^org^                    ^search^
^^^^^^^^---------------------------------------------------------
_c_: goto char      _h_: headline in buffer  _o_: helm-occur
_w_: goto word      _a_: heading in agenda   _p_: helm-swiper
_l_: goto line      _q_: swoop org buffers   _s_: swiper
^  ^                _t_: org-goto-target     _b_: search backward
-----------------------------------------------------------------
_u_: helm-buffers
_m_: helm-mini          _i_: ace-window
_r_: helm-recentf       _f_: find-file

_n_: Navigate           _d_: delete
"
  ("c" hydra-goto-char/body)
  ("w" hydra-goto-word/body)
  ("l" hydra-goto-line/body)

  ("i" ace-window)

  ("h" helm-org-headlines)
  ("a" helm-org-agenda-files-headings)
  ("q" helm-multi-swoop-org)

  ("o" helm-occur)
  ("p" swiper-helm)
  ("s" swiper)
  ("b" isearch-backward)

  ("u" helm-buffers-list)
  ("m" helm-mini)
  ("r" helm-recentf)
  ("f" helm-find-files)
  ("n" hydra-navigate/body)
  ("d" hydra-delete/body)
  ("t" org-goto-target))

(global-set-key (kbd "s-g") 'goto/body)


;; * modal delete
(defhydra hydra-delete (:color red :hint nil)
  "
_f_: delete-char             _w_: kill-word            _l_: kill-line
_b_: backward-delete-char    _W_: backward-kill-word   _L_: kill-line backward
_s_: kill-sentence           _p_: kill-paragraph       _x_: kill-sexp
_n_: navigate                _g_: goto
_C-d_: delete-char
"
  ("f" delete-char)
  ("C-d" delete-char)
  ("d" delete-char)
  ("b" backward-delete-char)
  ("w" kill-word)
  ("W" backward-kill-word)
  ("l" kill-line)
  ("L" (progn  (set-mark (point)) (beginning-of-line) (kill-region (point) (mark))))
  ("p" kill-paragraph)
  ("s" kill-sentence)
  ("x" kill-sexp)
  ("n" hydra-navigate/body :color blue)
  ("g" goto/body :color blue)
  ("q" nil :color blue))

(global-set-key (kbd "C-d") 'hydra-delete/body)


(defhydra hydra-git (:color blue :hint nil)
  "
_s_: status _c_: commit _p_: push _u_: pull
"
  ("s" magit-status)
  ("c" magit-commit)
  ("p" magit-push)
  ("u" magit-pull))

(defun git ()
  (interactive)
  (hydra-git/body))
