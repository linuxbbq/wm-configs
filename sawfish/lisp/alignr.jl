;; alignr.jl -- Another implementation for moving/resizing windows to align with others

;; (C) Vedat Hallac <vedat@hallac.net>, 14 July 2012

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; sawfish is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along
;; with sawfish; see the file COPYING.  If not, write to the Free Software
;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'rects)
(require 'maximize)
(current-head-offset)
(current-head-dimensions)

(define (make-alignr-commands)

  (define (from-hv-pair-select orientation pair)
    (define point-selector `((:vertical ,cdr)
                             (:horizontal ,car)))
    (funcall (cadr (assq orientation point-selector)) pair))

  (define (current-head-grid-points orientation)
    (let ((start (from-hv-pair-select orientation (current-head-offset)))
          (size  (from-hv-pair-select orientation (current-head-dimensions))))
      (list start (+ size start))))
      ;; Return start, 1/3 of screen, 1/2 of screen, 2/3 of screen, and end
      ;; (mapcar (lambda (x)
      ;;           (inexact->exact
      ;;            (round (+ start (* x size)))))
      ;;         (list 0 (/ 1.0 3) (/ 1.0 2) (/ 2.0 3) 1))))

(round (* 3 (/ 1 3)))
  (define (relevant-windows window)
    (remove-if (lambda (win)
                 (or (eq win window)
                     (window-iconified-p win)
                     (not (window-appears-in-workspace-p win current-workspace))
                     (window-outside-viewport-p win)))
               (managed-windows)))

  (define (grid-points-for window orientation)
    (define selectors `((:vertical ,cadr ,cadddr)
                        (:horizontal ,car ,caddr)))
    (uniquify-list
     (nconc (current-head-grid-points orientation)
            (let ((rects (rectangles-from-windows (relevant-windows window)))
                  (funcs (cdr (assq orientation selectors))))
              (unless (null funcs)
                (nconc (mapcar (car funcs) rects)
                       (mapcar (cadr funcs) rects)))))))

  (define (between pos min max)
    "t if min<=pos<=max"
    (and (>= pos min)
         (<= pos max)))

  (define (within pos target delta)
    "t if target-delta <= pos <= target or target >= pos >= target+delta"
    (or (between pos (- target delta) target)
        (between pos target (+ target delta))))

  (define (rect-side rect side)
    (cond ((eq side ':left) rect)
          ((eq side ':right) (cddr rect))
          ((eq side ':up) (cdr rect))
          ((eq side ':down) (cdddr rect))))

  (define (get-window-side window side)
    (let ((winrect (car (rectangles-from-windows (list window)))))
      (car (rect-side winrect side))))

  (define (get-window-frame-thickness window)
    (let ((frame-size (window-frame-dimensions window))
          (window-size (window-dimensions window)))
      (cons (- (car frame-size) (car window-size))
            (- (cdr frame-size) (cdr window-size)))))

  (define (set-window-side window side position)
    (let ((winrect (car (rectangles-from-windows (list window)))))
      (setcar (rect-side winrect side) position)
      (let* ((pos (cons (car winrect) (cadr winrect)))
             (thickness (get-window-frame-thickness window))
             (size (cons (- (caddr winrect) (car winrect) (car thickness))
                         (- (cadddr winrect) (cadr winrect) (cdr thickness)))))
        (maximize-truncate-dims window size
                                (if (eq (orientation-of side) ':horizontal)
                                    'horizontal
                                  'vertical))
        (move-resize-window-to window
                               (car pos) (cdr pos)
                               (car size) (cdr size)))))

  (define (get-min-increment window orientation)
    (let ((hints (window-size-hints window)))
      (or (cdr (assq
                (if (eq orientation ':horizontal)
                    'width-inc
                  'height-inc)
                hints)) 1)))

  (define (get-coord coord-pair orientation)
    (if (eq orientation ':horizontal)
        (car coord-pair)
      (cdr coord-pair)))

  (define (set-coord! coord-pair orientation new-value)
    (if (eq orientation ':horizontal)
        (setcar coord-pair new-value)
      (setcdr coord-pair new-value)))

  (define (orientation-of direction)
    (if (memq direction '(:left :right))
        ':horizontal
      ':vertical))

  (define (move-window window dir)
    (let ((orientation (orientation-of dir))
          (window-pos-pair (window-position window)))
      (when orientation
        (let ((window-pos (get-coord window-pos-pair orientation))
              (window-size (get-coord (window-frame-dimensions window) orientation))
              (screen-size (get-coord (cons (screen-width)
                                            (screen-height)) orientation))
              (grid-points (grid-points-for window orientation)))
          (nconc grid-points (mapcar (lambda (x) (- x window-size)) grid-points))
          (define (update-window min-pos max-pos selector-func)
            (let ((candidates (remove-if (lambda (x)
                                           (or (eql x window-pos)
                                               (not (between x min-pos max-pos))))
                                         grid-points)))
              (when candidates
                (set-coord! window-pos-pair orientation
                            (apply selector-func candidates))
                (move-window-to window (car window-pos-pair) (cdr window-pos-pair)))))
          (if (memq dir '(:left :up))
              (update-window 0 window-pos #'max)
            (update-window window-pos (- screen-size window-size) #'min))))))

  (define (move-window-side window side dir)
    (let ((orientation (orientation-of dir)))
      (when (and orientation
                 (eq orientation (orientation-of side)))
        (let ((side-pos (get-window-side window side))
              (screen-size (get-coord (cons (screen-width)
                                            (screen-height)) orientation))
              (grid-points (grid-points-for window orientation)))
          (define (update-window min-pos max-pos selector-func)
            (let* ((min-increment (get-min-increment window orientation))
                   (candidates (remove-if (lambda (x)
                                            (or (within x side-pos min-increment)
                                                (not (between x min-pos max-pos))))
                                          grid-points)))
              (when candidates
                (set-window-side window side
                                 (apply selector-func candidates)))))
          (if (memq dir '(:left :up))
              (update-window 0 side-pos #'max)
            (update-window side-pos screen-size #'min))))))

  (define (current-window)
    "Obtain the current window.
This function is lifted from sawfish/wm/commands.jl"
    (let ((win (current-event-window)))
      (if (or (null win) (eq win 'root))
          (input-focus)
        win)))

  ;; Make move commands...
  (define-command 'alignr-move-window-left move-window
    #:spec (lambda () (list (current-window) ':left)))
  (define-command 'alignr-move-window-right move-window
    #:spec (lambda () (list (current-window) ':right)))
  (define-command 'alignr-move-window-up move-window
    #:spec (lambda () (list (current-window) ':up)))
  (define-command 'alignr-move-window-down move-window
    #:spec (lambda () (list (current-window) ':down)))

  ;; Make grow commands
  (define-command 'alignr-grow-window-on-left move-window-side
    #:spec (lambda () (list (current-window) ':left ':left)))
  (define-command 'alignr-grow-window-on-right move-window-side
    #:spec (lambda () (list (current-window) ':right ':right)))
  (define-command 'alignr-grow-window-on-up move-window-side
    #:spec (lambda () (list (current-window) ':up ':up)))
  (define-command 'alignr-grow-window-on-down move-window-side
    #:spec (lambda () (list (current-window) ':down ':down)))

  ;; Make shrink commands
  (define-command 'alignr-shrink-window-from-left move-window-side
    #:spec (lambda () (list (current-window) ':left ':right)))
  (define-command 'alignr-shrink-window-from-right move-window-side
    #:spec (lambda () (list (current-window) ':right ':left)))
  (define-command 'alignr-shrink-window-from-up move-window-side
    #:spec (lambda () (list (current-window) ':up ':down)))
  (define-command 'alignr-shrink-window-from-down move-window-side
    #:spec (lambda () (list (current-window) ':down ':up))))

(make-alignr-commands)
