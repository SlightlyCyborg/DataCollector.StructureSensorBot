(ql:quickload :split-sequence)
(ql:quickload :inferior-shell)


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defvar cmds)


(setf cmds
      "<cmd> -t 59 --xpos 125 --ypos 0 -x 375 -y 175 -x 95 -y 160 -x 245 -y 125 -x 245 -y 235 
<cmd> -t 134 --xpos 85 --ypos 15 -x 250 -y 190 -x 255 -y 210 -x 260 -y 240 
<cmd> -t 58 --xpos 180 --ypos 10 -x 405 -y 235 -x 465 -y 170 -x 375 -y 315 -x 395 -y 275")

(setf cmds
      "<cmd> -t 59 --xpos 125 --ypos 0 -x 375 -y 175 -x 95 -y 160 -x 245 -y 125 -x 245 -y 235 
<cmd> -t 58 --xpos 180 --ypos 10 -x 405 -y 235 -x 465 -y 170 -x 375 -y 315 -x 395 -y 275")



(defun collect-data (cmds)
  (mapcar
   (lambda (i) (parse-integer (inferior-shell:run i :OUTPUT :STRING)))
   (mapcar
	      (lambda (item)
		(replace-all
		 item  "<cmd>" "IsThingPresent.StructureSensorBot"))
	      (split-sequence:split-sequence #\Newline cmds))))


(defun tell-me-to-run-if-late-and-still-at-my-desk ()
 (if (= 0 (first (collect-data)))))

(defun stats ()
  (mapcar
   (lambda (t-or-f response)
     (nth t-or-f response))
   (collect-data cmds)
   '(("You are not at your desk" "You are at your desk")
     ("You are not at the house" "You are are at the house")
     ("Your laptop is closed" "Your laptop is open"))))


(defun single-stats-write-iteration ()
  (let ((dat-file (open "room.dat" :if-does-not-exist :create :direction :output :if-exists :append)))
    (print
     (cons `(time . ,(multiple-value-list (get-decoded-time)))
	   (stats))
     dat-file)
    (close dat-file)))

(defun run-stats-loop ()
  (loop
     (single-stats-write-iteration)
     (sleep (* 5 60))))



