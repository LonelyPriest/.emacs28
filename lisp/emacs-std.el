;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs standard configure 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show title on emacs bar
(setq frame-title-format '("" buffer-file-name "@" user-login-name ":" system-name)) 

;; add new line at end of file
(setq require-final-newline t) 

;; disable bell when an error occured
(setq visible-bell t) 

;; disable startup message
(setq inhibit-startup-message t)

;; show line number
(setq column-number-mode t)

;; talbe == 4 space
(setq default-tab-width 4)
(setq-default indent-tabs-mode 0)

;; positioning text with the cursor position 
(setq mouse-yank-at-point t)

;; ring max
(setq kill-ring-max 20)

(setq enable-recursive-minibuffers t)

;;(setq default-major-mode 'text-mode)
(setq default-major-mode 'erlang-mode)

;; show another matched parenttheses when point to a parenttheses 
(setq show-paren-style 'parentheses)

;;mouse do not block the cursor
(mouse-avoidance-mode 'animate) 

;; do not genartor a backup file
(setq-default make-backup-file nil)

;; syntax highlighting
(global-font-lock-mode t)

;; recursive copy or delete directory using dired
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; jump to current dir
(global-set-key "\C-x\C-j" 'dired-jump)

;; show time
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

;; kill current buffer using C-x k
(global-set-key "\C-xk" 'kill-buffer)

(global-set-key "\C-c\C-v" 'view-mode)

;; delete the whole line when at the first of line
(setq-default kill-whole-line t)

(global-set-key "\M-o" 'other-window)

;; 
(setq resize-mini-windows nil)

;; do not use filename<?> when to have same file
(setq uniquify-buffer-name-style 'forward)

;; show key when use M-x command
(setq suggest-key-bindings 1)

(setq ring-bell-function 'ignore)

;; save bookmark when set it
(setq bookmark-save-flag t)

(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")

(setq user-full-name "buxianhui")
(setq user-mail-address "buxianhui@myowner.com")

;; show time
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
;;(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S chunyu")

(setq calendar-remove-frame-by-deleting t)
(setq calendar-week-start-day 1)
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)
(setq solar-holidays nil)

;; ansi color surpport when in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(setq confirm-kill-emacs #'yes-or-no-p)      ; �ڹر� Emacs ǰѯ���Ƿ�ȷ�Ϲرգ���ֹ��
(electric-pair-mode t)                       ; �Զ���ȫ����
(add-hook 'prog-mode-hook #'show-paren-mode) ; ���ģʽ�£������������ʱ������һ������
(global-auto-revert-mode t)                  ; ����һ�����޸����ļ�ʱ���� Emacs ��ʱˢ�� Buffer
(delete-selection-mode t)                    ; ѡ���ı��������ı����滻�ı�������������ϰ���˵������༭�����߼���
(setq make-backup-files nil)                 ; �ر��ļ��Զ�����
(add-hook 'prog-mode-hook #'hs-minor-mode)   ; ���ģʽ�£������۵������
; (global-display-line-numbers-mode 1)         ; �� Window ��ʾ�к�
(tool-bar-mode 0)                          ; �ر� Tool bar
(menu-bar-mode 0)
(when (display-graphic-p) (toggle-scroll-bar -1)) ; ͼ�ν���ʱ�رչ�����

;; font
(set-frame-font "Fira Code Retina-14")

;; Copy line from point to the end, exclude the line break
(defun qiang-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (point)
                  (line-end-position))
                  ;; (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))
(global-set-key (kbd "M-k") 'qiang-copy-line)

;; change the yes to y and no to n when answer the emacs question
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'emacs-std)
