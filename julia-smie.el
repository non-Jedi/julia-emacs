;;; julia-smie.el --- Julia parsing for indentation -*- lexical-binding: t -*-

(defun julia-mode--add-dots (ops)
  "Return copy of OPS extended with each element prepended with \".\"."
  (concatenate 'list ops (mapcar (lambda (op) (concat "." op)) ops)))

(eval-when-compile
  ;; TODO: use symbols instead of strings for operators?
  (defconst julia-mode-prec-assignment
    (concatenate 'list
                 '(":=" "$=")
                 (julia-mode--add-dots
                  '("=" "+=" "-=" "*=" "/=" "//=" "\\=" "^=" "÷=" "%=" "<<="
                    ">>=" ">>>=" "|=" "&=" "⊻=" "≔" "⩴" "≕" "~"))))
  ;; comma - higher than assignment outside parentheses, lower when inside
  (defconst julia-mode-prec-pair (julia-mode--add-dots '("=>")))
  (defconst julia-mode-prec-conditional '("?"))
  (defconst julia-mode-prec-arrow
    (concatenate
     'list
     '("--" "-->")
     (julia-mode--add-dots
      '("←" "→" "↔" "↚" "↛" "↞" "↠" "↢" "↣" "↦" "↤" "↮" "⇎" "⇍" "⇏" "⇐" "⇒" "⇔"
        "⇴" "⇶" "⇷" "⇸" "⇹" "⇺" "⇻" "⇼" "⇽" "⇾" "⇿" "⟵" "⟶" "⟷" "⟹" "⟺" "⟻" "⟼"
        "⟽" "⟾" "⟿" "⤀" "⤁" "⤂" "⤃" "⤄" "⤅" "⤆" "⤇" "⤌" "⤍" "⤎" "⤏" "⤐" "⤑" "⤔"
        "⤕" "⤖" "⤗" "⤘" "⤝" "⤞" "⤟" "⤠" "⥄" "⥅" "⥆" "⥇" "⥈" "⥊" "⥋" "⥎" "⥐" "⥒"
        "⥓" "⥖" "⥗" "⥚" "⥛" "⥞" "⥟" "⥢" "⥤" "⥦" "⥧" "⥨" "⥩" "⥪" "⥫" "⥬" "⥭" "⥰"
        "⧴" "⬱" "⬰" "⬲" "⬳" "⬴" "⬵" "⬶" "⬷" "⬸" "⬹" "⬺" "⬻" "⬼" "⬽" "⬾" "⬿" "⭀"
        "⭁" "⭂" "⭃" "⭄" "⭇" "⭈" "⭉" "⭊" "⭋" "⭌" "￩" "￫" "⇜" "⇝" "↜" "↝" "↩" "↪"
        "↫" "↬" "↼" "↽" "⇀" "⇁" "⇄" "⇆" "⇇" "⇉" "⇋" "⇌" "⇚" "⇛" "⇠" "⇢" "↷" "↶"
        "↺" "↻"))))
  (defconst julia-mode-prec-lazy-or '("||"))
  (defconst julia-mode-prec-lazy-and '("&&"))
  (defconst julia-mode-prec-comparison
    (concatenate
     'list
     '("<:" ">:" "in" "isa")
     (julia-mode--add-dots
      '(">" "<" ">=" "≥" "<=" "≤" "==" "===" "≡" "!=" "≠" "!==" "≢" "∈" "∉" "∋"
        "∌" "⊆" "⊈" "⊂" "⊄" "⊊" "∝" "∊" "∍" "∥" "∦" "∷" "∺" "∻" "∽" "∾" "≁" "≃"
        "≂" "≄" "≅" "≆" "≇" "≈" "≉" "≊" "≋" "≌" "≍" "≎" "≐" "≑" "≒" "≓" "≖" "≗"
        "≘" "≙" "≚" "≛" "≜" "≝" "≞" "≟" "≣" "≦" "≧" "≨" "≩" "≪" "≫" "≬" "≭" "≮"
        "≯" "≰" "≱" "≲" "≳" "≴" "≵" "≶" "≷" "≸" "≹" "≺" "≻" "≼" "≽" "≾" "≿" "⊀"
        "⊁" "⊃" "⊅" "⊇" "⊉" "⊋" "⊏" "⊐" "⊑" "⊒" "⊜" "⊩" "⊬" "⊮" "⊰" "⊱" "⊲" "⊳"
        "⊴" "⊵" "⊶" "⊷" "⋍" "⋐" "⋑" "⋕" "⋖" "⋗" "⋘" "⋙" "⋚" "⋛" "⋜" "⋝" "⋞" "⋟"
        "⋠" "⋡" "⋢" "⋣" "⋤" "⋥" "⋦" "⋧" "⋨" "⋩" "⋪" "⋫" "⋬" "⋭" "⋲" "⋳" "⋴" "⋵"
        "⋶" "⋷" "⋸" "⋹" "⋺" "⋻" "⋼" "⋽" "⋾" "⋿" "⟈" "⟉" "⟒" "⦷" "⧀" "⧁" "⧡" "⧣"
        "⧤" "⧥" "⩦" "⩧" "⩪" "⩫" "⩬" "⩭" "⩮" "⩯" "⩰" "⩱" "⩲" "⩳" "⩵" "⩶" "⩷" "⩸"
        "⩹" "⩺" "⩻" "⩼" "⩽" "⩾" "⩿" "⪀" "⪁" "⪂" "⪃" "⪄" "⪅" "⪆" "⪇" "⪈" "⪉" "⪊"
        "⪋" "⪌" "⪍" "⪎" "⪏" "⪐" "⪑" "⪒" "⪓" "⪔" "⪕" "⪖" "⪗" "⪘" "⪙" "⪚" "⪛" "⪜"
        "⪝" "⪞" "⪟" "⪠" "⪡" "⪢" "⪣" "⪤" "⪥" "⪦" "⪧" "⪨" "⪩" "⪪" "⪫" "⪬" "⪭" "⪮"
        "⪯" "⪰" "⪱" "⪲" "⪳" "⪴" "⪵" "⪶" "⪷" "⪸" "⪹" "⪺" "⪻" "⪼" "⪽" "⪾" "⪿" "⫀"
        "⫁" "⫂" "⫃" "⫄" "⫅" "⫆" "⫇" "⫈" "⫉" "⫊" "⫋" "⫌" "⫍" "⫎" "⫏" "⫐" "⫑" "⫒"
        "⫓" "⫔" "⫕" "⫖" "⫗" "⫘" "⫙" "⫷" "⫸" "⫹" "⫺" "⊢" "⊣" "⟂"))))
  (defconst julia-mode-prec-pipe< '("<|"))
  (defconst julia-mode-prec-pipe> '("|>"))

  (defconst julia-mode-prec-colon
    (concatenate 'list
                 '(":" "..")
                 (julia-mode--add-dots '("…" "⁝" "⋮" "⋱" "⋰" "⋯"))))
  (defconst julia-mode-prec-plus
    (concatenate
     'list '("$")
     (julia-mode--add-dots
      '("$" "+" "-" "|" "⊕" "⊖" "⊞" "⊟" "++" "∪" "∨" "⊔" "±" "∓" "∔" "∸" "≏" "⊎"
        "⊻" "⊽" "⋎" "⋓" "⧺" "⧻" "⨈" "⨢" "⨣" "⨤" "⨥" "⨦" "⨧" "⨨" "⨩" "⨪" "⨫" "⨬"
        "⨭" "⨮" "⨹" "⨺" "⩁" "⩂" "⩅" "⩊" "⩌" "⩏" "⩐" "⩒" "⩔" "⩖" "⩗" "⩛" "⩝" "⩡"
        "⩢" "⩣"))))
  (defconst julia-mode-prec-times
    (julia-mode--add-dots
     '("*" "/" "÷" "%" "&" "⋅" "∘" "×" "\\" "∩" "∧" "⊗" "⊘" "⊙" "⊚" "⊛" "⊠" "⊡"
       "⊓" "∗" "∙" "∤" "⅋" "≀" "⊼" "⋄" "⋆" "⋇" "⋉" "⋊" "⋋" "⋌" "⋏" "⋒" "⟑" "⦸"
       "⦼" "⦾" "⦿" "⧶" "⧷" "⨇" "⨰" "⨱" "⨲" "⨳" "⨴" "⨵" "⨶" "⨷" "⨸" "⨻" "⨼" "⨽"
       "⩀" "⩃" "⩄" "⩋" "⩍" "⩎" "⩑" "⩓" "⩕" "⩘" "⩚" "⩜" "⩞" "⩟" "⩠" "⫛" "⊍" "▷"
       "⨝" "⟕" "⟖" "⟗")))
  (defconst julia-mode-prec-rational (julia-mode--add-dots '("//")))
  (defconst julia-mode-prec-bitshift (julia-mode--add-dots '("<<" ">>" ">>>")))
  ;; `where`
  ;; implicit multiplication (juxtaposition)
  ;; unary
  (defconst julia-mode-prec-power
    (julia-mode--add-dots
     '("^" "↑" "↓" "⇵" "⟰" "⟱" "⤈" "⤉" "⤊" "⤋" "⤒" "⤓" "⥉" "⥌" "⥍" "⥏" "⥑" "⥔"
       "⥕" "⥘" "⥙" "⥜" "⥝" "⥠" "⥡" "⥣" "⥥" "⥮" "⥯" "￪" "￬")))
  (defconst julia-mode-prec-decl '("::"))
  ;; `where` occurring after `::`
  (defconst julia-mode-prec-dot '(".")))

(defconst julia-mode-prec-names
  '(julia-mode-prec-assignment julia-mode-prec-pair julia-mode-prec-conditional
    julia-mode-prec-arrow julia-mode-prec-lazy-or julia-mode-prec-lazy-and
    julia-mode-prec-comparison julia-mode-prec-pipe< julia-mode-prec-pipe>
    julia-mode-prec-colon julia-mode-prec-plus julia-mode-prec-times
    julia-mode-prec-rational julia-mode-prec-bitshift julia-mode-prec-power
    julia-mode-prec-decl julia-mode-prec-dot))

(defconst julia-mode--whitespace-chars
  (string 9 11 12 13 32 133 160 5760 6158 8192 8193 8194 8195 8196 8197 8198
          8199 8200 8201 8202 8232 8233 8239 8287 12288))
(defconst julia-mode--whitespace-chars+ (concat julia-mode--whitespace-chars "\n"))

(defconst julia-smie-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (lambda (ch) (modify-syntax-entry ch "-" table))
          julia-mode--whitespace-chars)
    table))

;; Implements function skipws/skip-ws assuming julia-smie-syntax-table
;; is current syntax table:
;; https://github.com/JuliaLang/JuliaParser.jl/blob/a3a152547a84748e266cbce330f64cd1820e0bde/src/lexer.jl#L373
(defun julia-mode--skip-ws (newlines)
  ;; TODO: wrap in (with-syntax-table julia-smie-syntax-table ...) ?
  (not (zerop (if newlines (skip-chars-forward julia-mode--whitespace-chars+)
                (skip-syntax-forward "-")))))

(defun julia-mode--next-token (s)
  "Return next token and move point to end of token given lexing state S.
Instead of operating on a string like the original does, this function
directly manipulates the emacs buffer."
  (aset s 2 (julia-mode--skip-ws t))
  ;; This follows JuliaParser.jl's next_token implementation for EOF checks
  (if (eobp)
      'EOF
    (let ((c (char-after)))))) ; TODO...

;;; token stream

;; element 1 of vector is the string/iobuffer in original parser. For
;; consistency, it will always remain nil in this implementation (see
;; julia-mode--next-token docstring).
(defun julia-mode--make-token-stream () (vector nil nil t nil nil))
;;(defmacro julia-mode--ts:port (s) `(aref ,s 1))
(defmacro julia-mode--ts:last-tok (s) `(aref ,s 0))
(defmacro julia-mode--ts:set-tok! (s tok) `(aset ,s 0 ,tok))
(defmacro julia-mode--ts:pbtok (s) `(aref ,s 3))
(defun julia-mode--ts:space? (s) (aref s (if (julia-mode--ts:pbtok s) 4 2)))
(defun julia-mode--ts:put-back! (s tok spc)
  (if (julia-mode--ts:pbtok s)
      (error "too many pushed-back tokens (internal error)")
    (aset s 3 tok)
    (aset s 4 spc)))
