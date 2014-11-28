#lang racket

;; Refer to LICENSE.txt for licensing information (GPL 3.0)

(require "compiler.rkt")
(require "interpreter.rkt")

;; Some tests here due to Daniel B. Cristofani <http://www.hevanet.com/cristofd/brainfuck/>.
;; Other tests due to <http://copy.sh/brainfuck/text.html>,
;; and <http://codegolf.stackexchange.com/questions/5418/brainfuck-golfer/5440#5440>.
;; No copyright or licensing information was included in any of these works;
;; I reproduce them with the most innocent of intentions.

(require rackunit)

;; "A"
(check-equal?
	(get-output-string (run
		(compile (open-input-string "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++."))
		(open-input-string "")
		(open-output-string)))
	"A")

;; Hello World
(check-equal?
	(get-output-string (run
		(compile (open-input-string (string-append
				"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---"
				".+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")))
		(open-input-string "")
		(open-output-string)))
	"Hello World!\n")

;; "sigkill"
(check-equal?
	(get-output-string (run
		(compile (open-input-string "+[--------->++<]>+.----------.--.++++.--.+++.."))
		(open-input-string "")
		(open-output-string)))
	"sigkill")

;; A paragraph
(check-equal?
	(get-output-string (run
		(compile (open-input-string (string-append
				"-[--->+<]>-.[---->+++++<]>-.---.--[--->+<]>-.----[->++++<]>+.++++."
				"------------.------.++++++++.-[++>---<]>+.[->+++<]>++.[--->+<]>----."
				"---.++++++++.---------.-[->+++++<]>-.++[->+++<]>.+++++++++.+++++++++."
				"[---->+<]>++.-[--->++<]>.+++++++++++.--------.+++.+++.+[---->+<]>+++."
				"+++++[->+++<]>.+++++++.+[->+++<]>.+++++++++++++.[-->+++++<]>+++.---[-"
				">++++<]>.------------.---.--[--->+<]>-.++[--->++<]>.-----------.+[-----"
				">+<]>.-.-[---->+<]>++.+[->+++<]>+.+++++++++++.--------.++++++++++++.+[++>"
				"---<]>.++[--->++<]>.+++[->++<]>.[++>-------<]>.---------..[++>---<]>--.>-["
				"--->+<]>---.[------>+<]>--.++.++++++++.------.[--->+<]>---.[---->+<]>+++.-["
				"--->++<]>-.++++++++++.+[---->+<]>+++.+[----->+<]>+.+.+++++.[---->+<]>+++.["
				"-->+++++++<]>.+++++.---.-------------.+++++++.-[--->+<]>.-[---->+<]>++.++[-"
				">+++<]>.-[--->+<]>--.-------.-----------.-[--->+<]>--.-----------.++++++.-."
				"-------------.+++++++++++.++[----->++<]>.------------.[->+++<]>+.+++++++++++"
				"..+++.++++++++.+[->+++<]>+.+++++.-------.-[--->+<]>--.---[->++++<]>-.++.+[->"
				"+++<]>+.+++++.--[--->+<]>--.[->+++<]>+.+.[--->+<]>---.+.--.+++[->+++<]>++.+"
				"+.-[--->+<]>--.[---->+<]>+++.+[->+++<]>.++++++++++++.-.-----------.++.+++++"
				"++++++.++++.-.+[---->+<]>+++.[->+++<]>+.--[--->+<]>--.+[---->+<]>+++.-[--->"
				"++<]>-.+++++.++.+++++.-.[---->+<]>+++.[->+++<]>+.+++++++++++++.----------.-["
				"--->+<]>-.+++++[->+++<]>.++++++.-.----.+++++.-.[++>---<]>.")))
		(open-input-string "")
		(open-output-string)))
	"The quick brown fox jumps over the lazy dogs. Full Racket is not purely functional, allowing such abstract concepts as input and output.")

;; echo
(check-equal?
	(get-output-string (run
		(compile (open-input-string ",[.[-],]"))
		(open-input-string "This program should echo every character it reads, halting after the first EOF but only after at least one read.")
		(open-output-string)))
	"This program should echo every character it reads, halting after the first EOF but only after at least one read.")

;; rot13
(check-equal?
	(get-output-string (run
		(compile (open-input-string (string-append
			",[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
			"[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
			"[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
			"[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
			"[>++++++++++++++<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
			"[>>+++++[<----->-]<<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-"
			"[>++++++++++++++<-[>+<-[>+<-[>+<-[>+<-[>+<-[>++++++++++++++<-[>+<-[>+<-[>+<-[>+<-"
			"[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>>+++++[<----->-]<<-[>+<-[>+<-[>+<-[>+<-"
			"[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>++++++++++++++<-[>+<-]]]]]]]]]]]]]]]]]]]]"
			"]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
			"]]]]]]]]]]]]]]]]]]]]>.[-]<,]")))
		(open-input-string "Gur dhvpx oebja sbk whzcf bire gur ynml qbtf. (519) 888-4567")
		(open-output-string)))
	"The quick brown fox jumps over the lazy dogs. (519) 888-4567")

;; collatz
(check-equal?
	(get-output-string (run
		(compile (open-input-string (string-append
			">,["
			"    ["
			"        ----------["
			"            >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]"
			"            ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<"
			"        ]>"
			"    ]>>>++>+>>["
			"        <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]"
			"        >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]"
			"        >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>"
			"    ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,"
			"]")))
		(open-input-string "13\n37\n42\n27\n9999999999\n")
		(open-output-string)))
	"9\n21\n8\n111\n230\n")

;; nonsense
(check-equal? (compile (open-input-string "")) empty)
(check-equal? (compile (open-input-string "this is nonsense")) empty)
(check-exn (lambda (x) (string=? (exn-message x) "unmatched brackets: too many ['s")) (lambda () (compile (open-input-string "["))))
(check-exn (lambda (x) (string=? (exn-message x) "unmatched brackets: too many ]'s")) (lambda () (compile (open-input-string "]"))))
