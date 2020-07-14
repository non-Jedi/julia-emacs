;;; julia-smie.el --- Julia parsing for indentation -*- lexical-binding: t -*-

(defun julia--add-dots (ops)
  "Return copy of OPS extended with each element prepended with \".\"."
  (append ops (mapcar (lambda (op) (intern (concat "." (symbol-name op)))) ops)))

;; TODO: use symbols instead of strings for operators?
(defconst julia-prec-assignment
  (append
   (julia--add-dots
    (mapcar #'intern
            '("=" "+=" "-=" "*=" "/=" "//=" "\\=" "^=" "÷=" "%=" "<<=" ">>=" ">>>=" "|=" "&=" "⊻=" "≔" "⩴" "≕" "~")))
   (mapcar #'intern '(":=" "$="))))
;; comma - higher than assignment outside parentheses, lower when inside
(defconst julia-prec-pair (julia--add-dots (list '=>)))
(defconst julia-prec-conditional (list '\?))
(defconst julia-prec-arrow
  (append
   '(-- -->)
   (julia--add-dots
    (mapcar #'intern
            (list "←" "→" "↔" "↚" "↛" "↞" "↠" "↢" "↣" "↦" "↤" "↮" "⇎" "⇍" "⇏" "⇐" "⇒" "⇔" "⇴" "⇶" "⇷" "⇸" "⇹" "⇺" "⇻" "⇼" "⇽" "⇾" "⇿" "⟵" "⟶" "⟷" "⟹" "⟺" "⟻" "⟼" "⟽" "⟾" "⟿" "⤀" "⤁" "⤂" "⤃" "⤄" "⤅" "⤆" "⤇" "⤌" "⤍" "⤎" "⤏" "⤐" "⤑" "⤔" "⤕" "⤖" "⤗" "⤘" "⤝" "⤞" "⤟" "⤠" "⥄" "⥅" "⥆" "⥇" "⥈" "⥊" "⥋" "⥎" "⥐" "⥒" "⥓" "⥖" "⥗" "⥚" "⥛" "⥞" "⥟" "⥢" "⥤" "⥦" "⥧" "⥨" "⥩" "⥪" "⥫" "⥬" "⥭" "⥰" "⧴" "⬱" "⬰" "⬲" "⬳" "⬴" "⬵" "⬶" "⬷" "⬸" "⬹" "⬺" "⬻" "⬼" "⬽" "⬾" "⬿" "⭀" "⭁" "⭂" "⭃" "⭄" "⭇" "⭈" "⭉" "⭊" "⭋" "⭌" "￩" "￫" "⇜" "⇝" "↜" "↝" "↩" "↪" "↫" "↬" "↼" "↽" "⇀" "⇁" "⇄" "⇆" "⇇" "⇉" "⇋" "⇌" "⇚" "⇛" "⇠" "⇢" "↷" "↶" "↺" "↻")))))
(defconst julia-prec-lazy-or '("||"))
(defconst julia-prec-lazy-and '("&&"))
(defconst julia-prec-comparison
  (append
   (list '<: '>: 'in 'isa)
   (julia--add-dots
    (mapcar #'intern
            '(">" "<" ">=" "≥" "<=" "≤" "==" "===" "≡" "!=" "≠" "!==" "≢" "∈" "∉" "∋" "∌" "⊆" "⊈" "⊂" "⊄" "⊊" "∝" "∊" "∍" "∥" "∦" "∷" "∺" "∻" "∽" "∾" "≁" "≃" "≂" "≄" "≅" "≆" "≇" "≈" "≉" "≊" "≋" "≌" "≍" "≎" "≐" "≑" "≒" "≓" "≖" "≗" "≘" "≙" "≚" "≛" "≜" "≝" "≞" "≟" "≣" "≦" "≧" "≨" "≩" "≪" "≫" "≬" "≭" "≮" "≯" "≰" "≱" "≲" "≳" "≴" "≵" "≶" "≷" "≸" "≹" "≺" "≻" "≼" "≽" "≾" "≿" "⊀" "⊁" "⊃" "⊅" "⊇" "⊉" "⊋" "⊏" "⊐" "⊑" "⊒" "⊜" "⊩" "⊬" "⊮" "⊰" "⊱" "⊲" "⊳" "⊴" "⊵" "⊶" "⊷" "⋍" "⋐" "⋑" "⋕" "⋖" "⋗" "⋘" "⋙" "⋚" "⋛" "⋜" "⋝" "⋞" "⋟" "⋠" "⋡" "⋢" "⋣" "⋤" "⋥" "⋦" "⋧" "⋨" "⋩" "⋪" "⋫" "⋬" "⋭" "⋲" "⋳" "⋴" "⋵" "⋶" "⋷" "⋸" "⋹" "⋺" "⋻" "⋼" "⋽" "⋾" "⋿" "⟈" "⟉" "⟒" "⦷" "⧀" "⧁" "⧡" "⧣" "⧤" "⧥" "⩦" "⩧" "⩪" "⩫" "⩬" "⩭" "⩮" "⩯" "⩰" "⩱" "⩲" "⩳" "⩵" "⩶" "⩷" "⩸" "⩹" "⩺" "⩻" "⩼" "⩽" "⩾" "⩿" "⪀" "⪁" "⪂" "⪃" "⪄" "⪅" "⪆" "⪇" "⪈" "⪉" "⪊" "⪋" "⪌" "⪍" "⪎" "⪏" "⪐" "⪑" "⪒" "⪓" "⪔" "⪕" "⪖" "⪗" "⪘" "⪙" "⪚" "⪛" "⪜" "⪝" "⪞" "⪟" "⪠" "⪡" "⪢" "⪣" "⪤" "⪥" "⪦" "⪧" "⪨" "⪩" "⪪" "⪫" "⪬" "⪭" "⪮" "⪯" "⪰" "⪱" "⪲" "⪳" "⪴" "⪵" "⪶" "⪷" "⪸" "⪹" "⪺" "⪻" "⪼" "⪽" "⪾" "⪿" "⫀" "⫁" "⫂" "⫃" "⫄" "⫅" "⫆" "⫇" "⫈" "⫉" "⫊" "⫋" "⫌" "⫍" "⫎" "⫏" "⫐" "⫑" "⫒" "⫓" "⫔" "⫕" "⫖" "⫗" "⫘" "⫙" "⫷" "⫸" "⫹" "⫺" "⊢" "⊣" "⟂")))))
(defconst julia-prec-pipe< (list '<|))
(defconst julia-prec-pipe> (list '|>))
(defconst julia-prec-colon
  (append
   (list ': '..)
   (julia--add-dots
    (mapcar #'intern
            '("…" "⁝" "⋮" "⋱" "⋰" "⋯")))))
(defconst julia-prec-plus
  (append
   '($)
   (julia--add-dots
    (mapcar #'intern
            '("$" "+" "-" "|" "⊕" "⊖" "⊞" "⊟" "++" "∪" "∨" "⊔" "±" "∓" "∔" "∸" "≏" "⊎" "⊻" "⊽" "⋎" "⋓" "⧺" "⧻" "⨈" "⨢" "⨣" "⨤" "⨥" "⨦" "⨧" "⨨" "⨩" "⨪" "⨫" "⨬" "⨭" "⨮" "⨹" "⨺" "⩁" "⩂" "⩅" "⩊" "⩌" "⩏" "⩐" "⩒" "⩔" "⩖" "⩗" "⩛" "⩝" "⩡" "⩢" "⩣")))))
(defconst julia-prec-times
  (julia--add-dots
   (mapcar #'intern
           '("*" "/" "÷" "%" "&" "⋅" "∘" "×" "\\" "∩" "∧" "⊗" "⊘" "⊙" "⊚" "⊛" "⊠" "⊡" "⊓" "∗" "∙" "∤" "⅋" "≀" "⊼" "⋄" "⋆" "⋇" "⋉" "⋊" "⋋" "⋌" "⋏" "⋒" "⟑" "⦸" "⦼" "⦾" "⦿" "⧶" "⧷" "⨇" "⨰" "⨱" "⨲" "⨳" "⨴" "⨵" "⨶" "⨷" "⨸" "⨻" "⨼" "⨽" "⩀" "⩃" "⩄" "⩋" "⩍" "⩎" "⩑" "⩓" "⩕" "⩘" "⩚" "⩜" "⩞" "⩟" "⩠" "⫛" "⊍" "▷" "⨝" "⟕" "⟖" "⟗"))))
(defconst julia-prec-rational (julia--add-dots '(//)))
(defconst julia-prec-bitshift (julia--add-dots '(<< >> >>>)))
;; `where`
;; implicit multiplication (juxtaposition)
;; unary
(defconst julia-prec-power
  (julia--add-dots
   (mapcar #' intern
              '("^" "↑" "↓" "⇵" "⟰" "⟱" "⤈" "⤉" "⤊" "⤋" "⤒" "⤓" "⥉" "⥌" "⥍" "⥏" "⥑" "⥔" "⥕" "⥘" "⥙" "⥜" "⥝" "⥠" "⥡" "⥣" "⥥" "⥮" "⥯" "￪" "￬"))))
(defconst julia-prec-decl '(::))
;; `where` occurring after `::`
(defconst julia-prec-dot '(.))

(defconst julia-prec-names
  '(julia-prec-assignment julia-prec-pair julia-prec-conditional
    julia-prec-arrow julia-prec-lazy-or julia-prec-lazy-and
    julia-prec-comparison julia-prec-pipe< julia-prec-pipe>
    julia-prec-colon julia-prec-plus julia-prec-times
    julia-prec-rational julia-prec-bitshift julia-prec-power
    julia-prec-decl julia-prec-dot))

(defconst julia-trans-op ".'")
(defconst julia-ctrans-op "'")
(defconst julia-vararg-op "...")

(defun julia--Set (l)
  "Construct a membership-tester specialized to length of L."
  (cond ((= (length l) 1)
         (eval `(lambda (x)
                  (eq x (quote ,(car l))))))
        ((not (> (length l) 20))
         (eval `(lambda (x)
                  (not (not (memq x (quote ,l)))))))
        (t
         (let ((table (make-hash-table :test eq
                                       :size (length l))))
           (dolist (x l) (puthash x t table))
           (lambda (x) (gethash x table nil))))))

(defconst julia--whitespace-chars
  (string 9 11 12 13 32 133 160 5760 6158 8192 8193 8194 8195 8196 8197 8198
          8199 8200 8201 8202 8232 8233 8239 8287 12288))
(defconst julia--whitespace-chars+ (concat julia--whitespace-chars "\n"))

(defconst julia-smie-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (lambda (ch) (modify-syntax-entry ch "-" table))
          julia--whitespace-chars)
    table))

;; Implements function skipws/skip-ws assuming julia-smie-syntax-table
;; is current syntax table:
;; https://github.com/JuliaLang/JuliaParser.jl/blob/a3a152547a84748e266cbce330f64cd1820e0bde/src/lexer.jl#L373
(defun julia--skip-ws (newlines)
  ;; TODO: wrap in (with-syntax-table julia-smie-syntax-table ...) ?
  (not (zerop (if newlines (skip-chars-forward julia--whitespace-chars+)
                (skip-syntax-forward "-")))))

(defun julia--next-token (s)
  "Return next token and move point to end of token given lexing state S.
Instead of operating on a string like the original does, this function
directly manipulates the emacs buffer."
  (aset s 2 (julia--skip-ws t))
  ;; This follows JuliaParser.jl's next_token implementation for EOF checks
  (if (eobp)
      'EOF
    (let ((c (char-after)))))) ; TODO...

;;; token stream

;; element 1 of vector is the string/iobuffer in original parser. For
;; consistency, it will always remain nil in this implementation (see
;; julia--next-token docstring).
(defun julia--make-token-stream () (vector nil nil t nil nil))
;;(defmacro julia--ts:port (s) `(aref ,s 1))
(defmacro julia--ts:last-tok (s) `(aref ,s 0))
(defmacro julia--ts:set-tok! (s tok) `(aset ,s 0 ,tok))
(defmacro julia--ts:pbtok (s) `(aref ,s 3))
(defun julia--ts:space? (s) (aref s (if (julia--ts:pbtok s) 4 2)))
(defun julia--ts:put-back! (s tok spc)
  (if (julia--ts:pbtok s)
      (error "too many pushed-back tokens (internal error)")
    (aset s 3 tok)
    (aset s 4 spc)))
