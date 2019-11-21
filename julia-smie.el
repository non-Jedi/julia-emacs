;;; julia-smie.el --- Julia parsing for indentation -*- lexical-binding: t -*-

(defun julia-mode--add-dots (ops)
  "Return copy of OPS extended with each element prepended with \".\"."
  (concatenate 'list ops (map 'list (lambda (op) (concat "." op)) ops)))

(eval-when-compile
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

;;; token stream

(defun julia-mode--make-token-stream (s) (vector nil s t nil nil))
(defmacro julia-mode--ts:port (s) `(aref ,s 1))
(defmacro julia-mode--ts:last-tok (s) `(aref ,s 0))
(defmacro julia-mode--ts:set-tok! (s tok) `(aset ,s 0 ,tok))
(defmacro julia-mode--ts:pbtok (s) `(aref ,s 3))
(defun julia-mode--ts:space? (s) (aref s (if (julia-mode--ts:pbtok s) 4 2)))
(defun julia-mode--ts:put-back! (s tok spc)
  (if (julia-mode--ts:pbtok s)
      (error "too many pushed-back tokens (internal error)")
    (aset s 3 tok)
    (aset s 4 spc)))
