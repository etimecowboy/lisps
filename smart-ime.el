;; By marxprogram (��������ũ)@newsmth.net
;; http://www.newsmth.net/nForum/#!article/Emacs/104186?p=2

;;;; ��Ҫ��������⣺
;; *) �������뷨���Ű������е����
;; ������뷨���ڿ���״̬�������emacs��ʹ�á�
;; ����C-x b��C-h t��������뷨���ڿ���״̬��b��t���������뷨��Ϊ�������ƴ�����Ӷ��������ִ��ڡ�
;; ����������밴������֮ǰ�Զ��ر����뷨�ͺ��ˡ�
;; *) ����������֮ǰ�Զ�����Ӣ��״̬
;; �������ִ��M-x version��Ϊ������version����Ͳ��ò��ر����뷨��������ڰ���M-x���Զ��ر����뷨�ͺ��ˡ�
;; *) ��ĳЩ�ض���buffer�н������뷨
;; ��*Org Agenda*����buffer�����кܶ�����ǵ��ַ���ʽ�������ַ��ᵼ�����ִ��ڳ��֡������������buffer�н������뷨�ͺ��ˡ�


;;;; ���������
;; *) �������뷨���Ű������е����
;; ����smart-ime-modeʱ��ǰ׺�����󶨵������ṩ�ġ�ǰ׺������������
;; ���԰���ǰ׺���󣬻ỽ��ǰ׺����������
;; �ú��������漸���£�
;;   1) �ر����뷨�༭��
;;   2) ȡ��ǰ׺���롰ǰ׺�����������İ󶨣���ǰ׺�����°󶨵�keymap��
;;   3) ��ǰ׺�������ͻ��¼����У��ͺ�������û�����ǰ׺��һ��
;; Ȼ�󣬰���ǰ׺�����ڵİ�������������һ��������
;; ����ִ����Ϻ��ٴν�ǰ׺���󶨵����ǵġ�ǰ׺������������
;; *) ����������֮ǰ�Զ�����Ӣ��״̬
;; M-x��ִ��execute-extended-commandǰ����ΪҪ������������ҲӦ��ʱ�ر����뷨
;; *) ��ĳЩ�ض���buffer�н������뷨
;; ��ÿ����������󶼼�⵱ǰ�����ĸ�buffer�У�������ڽ���IME��buffer�У��͹ر����뷨��
;; ���֮ǰIME���ڿ���״̬�����ٴδ�IME�����֮ǰIME���ڹر�״̬��ʲôҲ������


;;;; һЩʵ��ϸ��:
;; (ime-set-status arg) t��IME��nil�ر�IME
;; (ime-get-status) ȷ��IME�Ǵ򿪵Ļ��ǹرյģ�t��ʾ�򿪣�nil��ʾ�ر�
;; ������������ʵ����.c�����С�
;; ������ΪGNU Emacs��֧��Foreign Function Interface (FFI)��Ϊ��ʵ��������������Ҫ�޸�emacs��Դ�롣
;; �ر�˵����ͨ��(ime-set-status nil)�ر�IME����Ч������Shift���л���Ӣ��״̬��࣬��ͬ�����Ǹ����뷨��С�������������
;; Ҳ����˵�������߲���(ime-set-status t)�������������Σ���͵ð�����Ctrl-Space���������С�������³��֡�
;; ��һ���������ر�IME���ڶ������ٴμ���IME��
;; �ⶼ��windows�Ļ��ƣ��밳�޹ء�


;;;; ��֪�����⣺
;; ���ñ�ģʽ��Help�˵��е�C-h�������<f1>��Org�˵�����C-c��ͷ����ʾ��û�ˡ�
;; ������ΪC-h��C-c���󶨵���һ�������ϡ�
;; cua-modeҲ��ͬ�������⣬������ѡ��һƬ�����Org�˵�Ҳ�ᷢ��һ�������⡣
;; ����Ϊcua-mode��û�а�C-h������Help�˵��Ǻõġ�


;;;; ��������뷨��ص����⣺
;; �����õ����뷨��ʵ�кܶ�bug����Щbugֻ����ͨ������������뷨ʱ���ܱ�¶������
;; *) �ѹ����뷨
;; �汾��6.5
;; �����ѹ�ƴ�����뷨�е�һ��bug����һ�ιر����뷨��û���κ�Ч����
;; ��ʱ���˸�ɾ�����ִ����е���ĸ��Ȼ��C-g���ɣ��Ժ�������ˡ�
;; *) QQƴ��
;; �汾��4.5
;; ÿ���Զ��رտ������뷨���ᵼ�±�����״̬�ָ������������еĳ�ʼ״̬��
;; ����취���߼�����>�Զ��幦�ܣ���������г��򱣳�һ�µ���Ӣ�ġ�ȫ��Ǽ�������״̬��ǰ�ĸ�ѡ����ѡ�У��ƺ��Ӵ˾ͺ��ˡ�
;; *) ƴ���Ӽ�
;; �汾��5.2
;; ���ر�IMEʱ�����뷨��С������������ʧ����û���κβ���Ӱ�졣


;;;; ʵ��


(defvar ime-status nil)


(defun ime-save-and-set-status (arg)
  ;; �ڽ���IME��buffer�а���ǰ׺��ʱ����Ҫ����IME״̬��
  (when (not (is-no-IME-buffer-p))
    (setq ime-status (ime-get-status))
    )
  (ime-set-status arg)
  )


(defun ime-restore-status (tag)
  (ime-set-status ime-status)
  (when smart-ime--debug
    (message tag))
  )



(defvar smart-ime--state 'normal)
(defvar smart-ime--debug nil)
;; �Ƿ�����prefix-override-keymap�������ã�ǰ׺�����롰ǰ׺������������
(defvar smart-ime--ena-prefix-override-keymap nil)
;; prefix-override-keymap�����н�ǰ׺���󶨵���ǰ׺����������
(defvar smart-ime--prefix-override-keymap (make-sparse-keymap))

;; ����keymap�Ƿ��ѳ�ʼ��
(defvar smart-ime--keymaps-initialized nil)

;; ָ��ĳ��keymap���ĸ�ena����ʹ��
(defvar smart-ime--keymap-alist
  `(
    (smart-ime--ena-prefix-override-keymap . ,smart-ime--prefix-override-keymap)
    )
  )


;; ��ʼ��keymap
(defun smart-ime--init-keymaps ()
  ;; ���º���Ҫ�ر����뷨��ǰ׺����C-x��C-c��C-h
  (define-key smart-ime--prefix-override-keymap [(control x)] 'smart-ime--prefix-override-handler)
  (define-key smart-ime--prefix-override-keymap [(control c)] 'smart-ime--prefix-override-handler)
  (define-key smart-ime--prefix-override-keymap [(control h)] 'smart-ime--prefix-override-handler)
  )


;; ǰ׺��������
(defun smart-ime--prefix-override-handler (arg)
  "ǰ׺��������"
  (interactive "P")
  (smart-ime--prefix-override-replay arg))

(defun smart-ime--prefix-override-replay (arg)
  (let* ((keys (this-command-keys))
         (i (length keys))
         (key (aref keys (1- i))))

    ;; ��ʱ�ر����뷨����������ִ����Ϻ�ָ���
    (ime-save-and-set-status nil)

    ;; ����ִ�����ʱִ�й��Ӻ���smart-ime--post-command-handler
    ;; ע�⣬�ù��Ӻ���һ�������ϣ���ǰ׺������������Ϻ�ͻᱻ����
    (add-hook 'post-command-hook 'smart-ime--post-command-handler)
    (setq smart-ime--state 'prefix)

    ;; ȡ��ǰ׺���롰ǰ׺�����������İ󶨣��ָ�Ĭ��״̬
    (setq smart-ime--ena-prefix-override-keymap nil)

    ;; ��ǰ׺���ͻ��¼�����
    ;; Don't record this command
    (setq this-command last-command)
    ;; Restore the prefix arg
    (setq prefix-arg arg)
    (reset-this-command-lengths)
    ;; Push the key back on the event queue
    (setq unread-command-events (cons key unread-command-events))))


;; (defun smart-ime--pre-command-handler-1 ()
;;   )

;; (defun smart-ime--pre-command-handler ()
;;   (when smart-ime-mode
;;     (condition-case nil
;;         (smart-ime--pre-command-handler-1)

;;         ;; (progn (add-log-entry "pre command %d\n" pre-count)
;;         ;;        (setq pre-count (1+ pre-count)))

;;       (error nil))))


;; ����ִ�н�����ִ�еĶ���
(defun smart-ime--post-command-handler-1 ()
  (cond ((eq smart-ime--state 'prefix)
         (setq smart-ime--state 'sequence))


        ((eq smart-ime--state 'sequence)
         ;; �ָ����뷨״̬
         (ime-restore-status "1")
         ;; �ָ�ǰ׺���롰ǰ׺�����������İ�
         (setq smart-ime--ena-prefix-override-keymap t)
         (setq smart-ime--state 'normal)

         (remove-hook 'post-command-hook 'smart-ime--post-command-handler)
         ;;(setq ime-status)
         )

        (t
         (error "error state"))))


(defun smart-ime--post-command-handler ()
  (when smart-ime-mode
    (condition-case nil
        (smart-ime--post-command-handler-1)
      ;; (progn (add-log-entry "post command %d\n" post-count)
      ;;        (setq post-count (1+ post-count)))
      (error nil))))


;; �������minibuffer-setup-hook��minibuffer-exit-hook����
(defadvice execute-extended-command (around manage-ime
                                            (prefixarg))
  "Ignore case in `foo'."
  ;; ��ʱ�ر����뷨����������ִ����Ϻ�ָ���
  (ime-save-and-set-status nil)

  ad-do-it

  ;; �ָ����뷨״̬
  (ime-restore-status "2")
)


(defvar no-IME-buffer-list '("*Org Agenda*" "*Buffer List*") "��buffer�����ַ������list�ͱ��������buffer�н���IME")


(defun is-no-IME-buffer-p ()
  (member (buffer-name) no-IME-buffer-list)
  )


;; ��ĳЩ�ض���buffer�У�Ҫ�ر�IME����Щbuffer��Ϊno-IME-buffer��������IME��buffer��
;; Ϊ���ܰ�Ԥ����������������ȷ��smart-ime--post-command-no-IME-buffer-handler��smart-ime--post-command-handler֮�󱻻���
;; ����add-hook��˳�����Ҫ��
(defun smart-ime--post-command-no-IME-buffer-handler ()
  (when (eq smart-ime--state 'normal)
    (if (is-no-IME-buffer-p)
        (ime-set-status nil)
      (when (not (minibufferp)) 
        (when ime-status
          (ime-set-status t))
        )
      )
    )
  )


(define-minor-mode smart-ime-mode
  "Toggle Smart IME mode."
  :init-value nil
  :lighter " SmartIME"
  :global t

  (unless smart-ime--keymaps-initialized
    (smart-ime--init-keymaps)
    (setq smart-ime--keymaps-initialized t))

  ;; (if smart-ime-mode
  ;;     (progn
  ;;       (add-hook 'pre-command-hook 'smart-ime--pre-command-handler)
  ;;       (add-hook 'post-command-hook 'smart-ime--post-command-handler))
  ;;   (remove-hook 'pre-command-hook 'smart-ime--pre-command-handler)
  ;;   (remove-hook 'post-command-hook 'smart-ime--post-command-handler))

  (when smart-ime-mode
    (add-hook 'post-command-hook 'smart-ime--post-command-no-IME-buffer-handler)
    )

  (unless smart-ime-mode
    (remove-hook 'post-command-hook 'smart-ime--post-command-handler)
    (remove-hook 'post-command-hook 'smart-ime--post-command-no-IME-buffer-handler)
    )


  (if (not smart-ime-mode)
      (setq emulation-mode-map-alists (delq 'smart-ime--keymap-alist emulation-mode-map-alists))
    (add-to-ordered-list 'emulation-mode-map-alists 'smart-ime--keymap-alist 400)
    (setq smart-ime--ena-prefix-override-keymap t))

  (if smart-ime-mode
      (progn
        (ad-enable-advice 'execute-extended-command 'around 'manage-ime))
    (ad-disable-advice 'execute-extended-command 'around 'manage-ime))
  (ad-activate 'execute-extended-command) ; ����enable����disable������Ҫ����һ��activate
  ;;(ad-deactivate 'execute-extended-command)


  )


;;;; ������
;; �������н����󣬲Żᴥ��һ��post-command-hook



;;; Announce

(provide 'smart-ime)



(defun smart-ime-debug ()
  "Toggle SMART-IME debugging."
  (interactive)
  (setq smart-ime--debug (not smart-ime--debug)))
