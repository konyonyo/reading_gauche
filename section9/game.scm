;; -*- coding: utf-8 -*-

(use util.match)

(define *dungeon*
    '(["あなたは森の北側にいる。道は南に続いている" (s . 1)]
      ["あなたは鬱蒼とした森の中の道にいる。道は南北に伸びている。東に降りてゆく小道がある。"
          (n . 0)
          (s . 2)
          (e . 3)]
      ["足元がぬかるんでいる。道は直角に折れ、北と西に伸びている。西に続く道の先が明るくなっている。"
          (n . 1)
          (w . 4)]
      ["あなたは沼のほとりにいる。空気の動きが止まり、暑さを感じる。西に昇っていく小径がある。"
          (w . 1)]
      ["突然目の前が開けた。あなたは森の中の広場にいる。丈の短い、柔らかそうな草が一面に広場を覆っている。道が東に伸びている。"
          (e . 2)]))

(define (describe)
    (print (car (get-position *player*)))
    #t)

(define (move! direction)
    (let ((position (get-position *player*)))
        (cond
            [(assoc direction (cdr position))
                => (lambda (p)
                       (set-position! *player* (list-ref *dungeon* (cdr p)))
                       (describe))]
            [else (print "そちらには移動できません")]))
    #t)

(define (status)
    (print "hp :" (get-hp *player*))
    (print "mp :" (get-mp *player*))
    (print "inventory :" (get-inventory *player*))
    #t)

(define (reset!)
    (set! *player*
        (make-player 'hp 320 'mp 66 'position (car *dungeon*)
            'inventory '(potion potion dagger cokkie dagger)))
    #t)

(define (make-player . args)
    (let loop ((lis args))
        (match lis
            [() '()]
            [(attr value . rest) (cons (cons attr value) (loop rest))])))

(define *player* (make-player))

(define (get-hp player)
    (cdr (assoc 'hp player)))

(define (get-mp player)
    (cdr (assoc 'mp player)))

(define (get-inventory player)
    (cdr (assoc 'inventory player)))

(define (get-position player)
    (cdr (assoc 'position player)))

(define (set-position! player position)
    (set! (cdr (assoc 'position player)) position))
