(require 'json)
(require 'org)
(require 'org-element)
(require 'seq)

(defconst appid "Z9Ke3Ynhw8FchJ0SQBn1S7fxPHp8dKhHhBzIR53r")
(defconst query-url (concat
                     "https://api.nal.usda.gov/fdc/v1/foods/search?api_key="
                     appid
                     "&query="
                     )
  )

(defvar food-org-file "/home/alex/Notes/foodLog.org")

(defun query-food-data (query)
  "retrieves and parse json for a search of a particular query of the fdc food database"
  (let ( (url-buffer (url-retrieve-synchronously (concat query-url query) nil nil 30.0))
         (prev-buffer ( current-buffer ) ))
    (if url-buffer
        (progn
          (message "fuck")
          (set-buffer url-buffer)
        (let ((json-out (json-read))) 
          (progn
            ( set-buffer prev-buffer )
            json-out)
          )
        )
      (error "failed to contact food database")
      )
    )
  )


(defun org-lookup-food ()
  (interactive
   (let* ((query (read-string "What food do you want to add?") )
          (foods (gethash "foods"  (query-food-data (query))))
          (food_name (completing-read "Which of these do you want to use?"
                                      (mapcar (lambda (x) (gethash "description" x)) foods )))
          (food (seq-find (lambda (x) (equal (gethash "description" x) food_name) ) ))
          )
     (let* ((get-value (lambda (name)
                         (gethash "value" (seq-find (lambda (x)
                                                          (equal name))
                                                    nutrients ))
                         )
                       )
            (name ( to-identifier (gethash "description" food) ))
            (nutrients (gethash "foodNutrients"))
            (/ calories (get-value "Energy") 100)
            (/ fat (get-value "Total lipid (fat)") 100)
            (/ carbs (get-value "Carbohydrate, by difference") 100)
            (/ protein (get-value "Protein") 100)
            )
       (org-write-food name calories carbs fat protein)
       )
     )
   )
  )


(defun org-write-food (name calories carbs fat protein)
  (interactive "sEnter food name:\nnEnter calores:\nnEnter carbs:\nnEnter fat:\nnEnter protein:")
  (with-current-buffer (find-file-noselect food-org-file)
    (let* ((ast (org-element-parse-buffer))
          ( headline (org-element-map ast 'headline
        (lambda (headline)
          (progn
            ( and (equal "Foods" (car (org-element-property :title headline) ))
              headline) ))
        nil t) ))
      (progn
        (if (eq nil headline)
            (progn
              (goto-char 0)
              (insert "* Foods\n"))
          (progn
            (goto-char (org-element-property :begin headline))
            (forward-line 1))
          )
        (insert
          (format "** %s\n" name)
          (format "#+NAME: %s\n" name)
          (format "| Food | %s |\n" name)
          (format "|------+----|\n")
          (format "| Calories |  %f |\n" calories)
          (format "| Carbs | %f |\n" carbs)
          (format "| Fat | %f |\n" fat)
          (format "| Protein | %f |\n" protein)
        )
        (forward-line -1)
        (org-table-align)
        (message (concat "Food item added with name: " name))
      )
    )
  )
)


(defun to-identifier (str)
  (apply 'string
          (seq-map
           (lambda (x)
             (if (eq x 32)
                 95
                x
               )
             )
           (seq-filter
            (lambda (x)
              (or (and (<= 64 x) (<= x 122) ) (eq x 32)))
            str )
           )
          ))





(defun test-query ()
  (princ (query-food-data "cashew") xbuff)
  (switch-to-buffer xbuff))

