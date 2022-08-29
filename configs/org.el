;;; org.el -*- lexical-binding: t; -*-

(load "~/.doom.d/configs/helpers/prettify-utils")

(defun albusshin/init-org ()
  (setq-default
    org-directory                      (concat private-directory "/org")
    albusshin/org-file/achivements     (concat org-directory "/achievements/achievements.org")
    albusshin/org-file/journal         (concat org-directory "/journal.org")
    albusshin/org-file/success         (concat org-directory "/success.org")
    albusshin/org-file/fitness         (concat org-directory "/fitness.org")
    albusshin/org-file/inbox           (concat org-directory "/inbox.org")
    albusshin/org-file/insights        (concat org-directory "/insights.org")
    albusshin/org-file/reminders       (concat org-directory "/reminders.org")
    albusshin/org-file/relationship    (concat org-directory "/relationship.org")
    albusshin/org-file/someday         (concat org-directory "/someday.org")
    albusshin/org-file/tasks           (concat org-directory "/tasks.org")
    albusshin/org-file/trading         (concat org-directory "/trading.org")
  )

  (defun albusshin/open-inbox-org-file ()
    (interactive) (find-file albusshin/org-file/inbox))

  (defun albusshin/open-journal-org-file ()
    (interactive) (find-file albusshin/org-file/journal))

  (defun albusshin/open-reminders-org-file ()
    "Open reminders org file"
    (interactive) (find-file albusshin/org-file/reminders))

  (defun albusshin/open-someday-org-file ()
    (interactive) (find-file albusshin/org-file/someday))

  (defun albusshin/open-tasks-org-file ()
    "Open tasks org file"
    (interactive) (find-file albusshin/org-file/tasks))

  ;; org capture templates
  (setq org-capture-templates
        '(
          ("i" "New Inbox Item" entry (file albusshin/org-file/inbox)
           "* TODO %?\n%U")
          ("r" "New Reminder Item" entry (file+headline albusshin/org-file/reminders "Reminders")
           "* [#A] %?\nSCHEDULED: %t\n%U")
          ("j" "New Journal Item"  entry (file+olp+datetree albusshin/org-file/journal)
           "* %U %?")
          ))

  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              ":PROPERTIES:
:CREATED: %U
:END:
#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "Website-Based References" plain
           "%?"
           :if-new (file+head "reference/${slug}.org"
                              ":PROPERTIES:
:CREATED: %U
:ROAM_REFS: ${url}
:END:
#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("c" "cite" plain
           "%?"
           :if-new (file+head "reference/${citekey}-${slug}.org"
                              ":PROPERTIES:
:CREATED: %U
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                      :immediate-finish t
                      :unnarrowed t)
          ("p" "people" plain
           "%?"
           :if-new (file+head "people/${slug}.org"
                              ":PROPERTIES:
:CREATED: %U
:END:
#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org"
                      ":PROPERTIES:
:CREATED: %U
:END:
#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))

  (defun jethro/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref
                        )))
                        ;; :multiple nil :rebuild-cache t)))
    (let ((title (citar-format--entry (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
:CREATED: %U
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file))))

  (defun albusshin/new-inbox-item ()
    (interactive)
    (org-capture nil "i"))

  (defun albusshin/new-reminder-item ()
    (interactive)
    (org-capture nil "r"))

  (defun albusshin/journal-item ()
    (interactive)
    (org-capture nil "j"))

  (defun albusshin/save-all-org-buffers ()
    (interactive)
    (org-save-all-org-buffers))

  (defun albusshin/make-new-capture-inbox ()
    "Create a new frame and run org-capture with inbox item."
    (interactive)
    (albusshin/new-inbox-item))

  ;; Initialize org
  (setq org-default-notes-file albusshin/org-file/inbox)
  (setq org-agenda-window-setup 'current-window)
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "WONTFIX")))
  (setq org-agenda-skip-scheduled-if-done 't)
  (setq org-agenda-span 7)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-prefix-format
        '((agenda . "%i %-12:c%?-12t% s %b")
          (todo . " %i %-12:c %b")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-refile-targets '((albusshin/org-file/tasks :level . 1)
                             (albusshin/org-file/someday :level . 1)
                             (albusshin/org-file/trading :level . 1)
                             (albusshin/org-file/reminders :level . 1)
                             (albusshin/org-file/insights :level . 1)))
  (setq org-agenda-custom-commands
        '(("o" "At the office" tags-todo "@office"
           ((org-agenda-overriding-header "@office")))
          ("c" "In front of computer" tags-todo "@computer"
          ((org-agenda-overriding-header "@computer")))
          ("b" "During business hours" tags-todo "@BH"
           ((org-agenda-overriding-header "@BH")))
          ("h" "At home" tags-todo "@home"
           ((org-agenda-overriding-header "@home")))
          ("p" "Personal" tags-todo "personal"
           ((org-agenda-overriding-header "@phone")))
          ("l" "Leaving" tags-todo "When leaving from a place (e.g. leaving home for work)"
           ((org-agenda-overriding-header "@leaving")))
          ("w" "Work" tags-todo "work"
           ((org-agenda-overriding-header "work")))
          ))


  ;; Org roam customizations

  (setq-default org-roam-completion-everywhere 't)

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  ;; Org Beautify

  (setq org-startup-indented t
        org-src-tab-acts-natively t)

  (add-hook 'org-mode-hook
            (lambda ()
              (variable-pitch-mode 1)
              (visual-line-mode)
              ))

  (setq org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t)

  (setq org-ellipsis "  ⤵")

  (setq-default prettify-symbols-alist
                (prettify-utils-generate
                 ("[ ]"            "\s\s\s\s")
                 ("[X]"            "\s\s\s\s")
                 ("[-]"            "\s\s\s\s")
                 ("[#A]"           "\s\s🅰️️\s\s")
                 ("[#B]"           "\s\s⭐️️\s\s")
                 ("[#C]"           "\s\s☕️\s\s")
                 ("=>"             "➡️\s")
                 ("->"             "→\s")
                 (":PROPERTIES:"   "")
                 (":END:"          "―")
                 ("#+BEGIN_SRC"    "\s\s")
                 ("#+begin_src"    "\s\s")
                 ("#+END_SRC"      "―")
                 ("#+end_src"      "―")
                 ("#+title: "      "§\s\s")
                 ("#+TITLE: "      "§\s\s")
                 ("#+ROAM_TAGS:"   "\s\s")
                 ("#+roam_tags:"   "\s\s")
                 ("#+FILETAGS:"    "\s\s")
                 ("#+filetags:"    "\s\s")
                 ("SCHEDULED:"     "\s")
                 ("DEADLINE:"      "\s")))

  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-+]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "➤"))))))

  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (if (display-graphic-p (selected-frame))
    (add-hook 'org-mode-hook 'prettify-symbols-mode))

)
