;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://www.berkshire.net/~norm/dsssl/
;;

;; need test cases to do toc/lot; do these later

(element toc (empty-sosofo))
(element (toc title) (empty-sosofo))
(element tocfront ($paragraph$))
(element tocentry ($paragraph$))
(element tocpart (process-children))
(element tocchap (process-children))
(element toclevel1 (process-children))
(element toclevel2 (process-children))
(element toclevel3 (process-children))
(element toclevel4 (process-children))
(element toclevel5 (process-children))
(element tocback ($paragraph$))
(element lot (empty-sosofo))
(element (lot title) (empty-sosofo))
(element lotentry ($paragraph$))

