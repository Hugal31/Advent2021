(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (spec utils))

(define simple-example
  "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(define example-list
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(simple-example-suites
 8
 `((,example-list . 26))
 `((,simple-example . 5353)
   (,example-list . 61229))
 #:parse? #t)

(define eliminate-possibilities (@@ (days day08) eliminate-possibilities))

(suite "Test day 8 utils"
       (tests
        (test "eliminate-possibilities should find easy digits"
              e
              (assert-equal
               '((5 6)
                 ()
                 (2)
                 ()
                 ()
                 (3)
                 ())
               (eliminate-possibilities '((2 3))
                                        '((5 6 2)
                                          ()
                                          (1 2)
                                          ()
                                          ()
                                          (3 4)
                                          ()))))
        ;; (test "eliminate-possibilities should work on identity"
        ;;       e
        ;;       (assert-equal
        ;;        '((0)
        ;;          (1)
        ;;          (2)
        ;;          (3)
        ;;          (4)
        ;;          (5)
        ;;          (6))
        ;;        (eliminate-possibilities '((2 5) (0 2 5) (1 2 3 5) (0 2 3 5 6))
        ;;                                 '((0 1 2 3 4 5 6)
        ;;                                   (0 1 2 3 4 5 6)
        ;;                                   (0 1 2 3 4 5 6)
        ;;                                   (0 1 2 3 4 5 6)
        ;;                                   (0 1 2 3 4 5 6)
        ;;                                   (0 1 2 3 4 5 6)
        ;;                                   (0 1 2 3 4 5 6))))
        ;;       )
        )
       )
