;;; configs/base.el -*- lexical-binding: t; -*-

(setq private-directory              (expand-file-name "~/Dropbox/.private"))

(setq finance-directory                              (concat private-directory "/finance")
      finance-journal-directory                      (concat finance-directory "/journal")
      albusshin/finance-file/main                    (concat finance-journal-directory "/main.ledger")
      albusshin/finance-file/journal-CFU             (concat finance-journal-directory "/journal.CFU.ledger")
      albusshin/finance-file/journal-CSR             (concat finance-journal-directory "/journal.CSR.ledger")
      albusshin/finance-file/journal-chase-checking  (concat finance-journal-directory "/journal.chase.checking.ledger")
      albusshin/finance-file/journal-chase-savings   (concat finance-journal-directory "/journal.chase.savings.ledger")
      albusshin/finance-file/journal-hsbc-checking   (concat finance-journal-directory "/journal.hsbc.checking.ledger")
      albusshin/finance-file/journal-hsbc-savings    (concat finance-journal-directory "/journal.hsbc.savings.ledger")
      albusshin/finance-file/journal-investment      (concat finance-journal-directory "/journal.investment.ledger"))

(setq lifelog-directory                              (concat private-directory "/lifelog")
      albusshin/lifelog-file/now-timeclock           (concat lifelog-directory "/lifelog.now.timeclock")
      albusshin/lifelog-file/now-ledger              (concat lifelog-directory "/lifelog.now.ledger")
      albusshin/lifelog-file/ledger                  (concat lifelog-directory "/lifelog.ledger"))
