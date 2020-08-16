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
         )
    (if url-buffer
        (with-current-buffer url-buffer
          (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'string))
            (json-read))
        )
      (error "failed to contact food database")
      )
    )
  )


(defun org-lookup-food ()
  "Attempts to download the nutritional information for a food and add it to the food org file"
  (interactive
   (let* ((query (read-string "What food do you want to add?") )
           (foods (gethash "foods"  (query-food-data query)))
          (food_name (completing-read "Which of these do you want to use?"
                                      (mapcar (lambda (x) (gethash "description" x)) foods )))
          (food (seq-find (lambda (x) (equal (gethash "description" x) food_name) ) foods ))
          )
     (let* ((name ( to-identifier (gethash "description" food) ))
            (nutrients (gethash "foodNutrients" food ))
            (get-value (lambda (name)
                         (gethash "value" (seq-find (lambda (x)
                                                      (equal name (gethash "nutrientName" x)))
                                                    nutrients ))
                         )
                       )
            (calories (/ (funcall get-value "Energy") 100))
            ( fat (/ (funcall get-value "Total lipid (fat)") 100))
            ( carbs (/ (funcall get-value "Carbohydrate, by difference") 100))
            ( protein (/ (funcall get-value "Protein") 100))
            )
       (org-write-food name calories carbs fat protein)
       () 
       )
     )
   )
  )


(defun org-write-food (name calories carbs fat protein)
  "Adds a food and its information to the food org file"
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
        (message "%s" (concat "Food item added with name: " name))
        ()
      )
    )
  )
  )

(defconst food-table-regex "^\\|[:space:]*![:space:]*\\|[:space:]*Time[:space:]*\\|[:space:]*Food[:space:]*\\|[:space:]*Ammount[:space:]* \\|[:space:]*Calories[:space:]*\\|[:space:]*Carbs[:space:]*\\|[:space:]*Fat[:space:]*\\|[:space:]*Protein[:space:]*\\|[:space:]*Calorie Total[:space:]*\\|[:space:]*Carb Total[:space:]*\\|[:space:]*Fat Total[:space:]*\\|[:space:]*Protein Total[:space:]*\\|$")

(defconst food-table-head "| ! | Time                   | Food             | Ammount | Calories |     Carbs |       Fat |   Protein | Calorie Total | Carb Total | Fat Total | Protein Total |\n|---+------------------------+------------------+---------+----------+-----------+-----------+-----------+---------------+------------+-----------+---------------|\n")

(defconst food-table-formulas "#+TBLFM: $5=$4*remote($3,@2$2)::$6=$4*remote($3,@3$2)::$7=$4*remote($3,@4$2)::$8=$4*remote($3,@5$2)::$9=vsum(@2$Calories..@@#$Calories)::$10=vsum(@2$Carbs..@@#$Carbs)::$11=vsum(@2$Fat..@@#$Fat)::$12=vsum(@2$Protein..@@#$Protein)")


(defun org-log-food ()
  "Logs a food entry in the food org file"
  (interactive
   (with-current-buffer (find-file-noselect food-org-file)
     (let* ((ast (org-element-parse-buffer))
            (foods-headline (org-element-map ast 'headline
                         (lambda (headline)
                           (progn
                             ( and (equal "Foods" (car (org-element-property :title headline) ))
                                   headline) ))
                         nil t) )
            (foods (org-element-map ast 'headline
                     (lambda (headline)
                       (progn
                         ( and (eq foods-headline (org-element-property :parent headline) )
                               headline) ))
                     ))
            (selection (completing-read "Which food? " (seq-map (lambda (x) (org-element-property :raw-value x)) foods)))
            (ammount   (read-number "How many grams?"))
            (foods-log-headline (org-element-map ast 'headline
                              (lambda (headline)
                                (progn
                                  ( and (equal "Food Log" (org-element-property :raw-value headline) )
                                        headline) ))
                              nil t) )
            (time-stamp (with-temp-buffer (org-insert-time-stamp (current-time))
                                          (buffer-string)
                                          )
                        )
            (food-entry-raw (concat "Food for " time-stamp))
            (add-new-entry nil))
       (progn
         ;; Check if food headline exists and add it if it does not
         (if (eq nil foods-log-headline)
             (progn
               (org-element-insert-before)
               ;;add it at the end of the file
               (goto-char (point-max))
               (insert "\n* Food Log\n"
                       "** " food-entry-raw "\n"
                       food-table-head
                       "\n"
                       food-table-formulas)
               (forward-line -1)
               (goto-char (line-beginning-position))
               (setq add-new-entry t))
           ;; food headline exists so check if an entry exists
           (progn
            (let* ((food-entry-headline (org-element-map ast 'headline
                                          (lambda (headline)
                                            (progn
                                              ( and (equal food-entry-raw (org-element-property :raw-value headline))
                                                    (eq foods-log-headline (org-element-property :parent headline))
                                                    headline
                                                    )
                                              )
                                            )
                                          nil t))
                    )
              (if food-entry-headline
                  ;; found entry
                  (progn
                    (goto-char  (org-element-property :contents-begin food-entry-headline) )
                    (if (search-forward-regexp food-table-regex (org-element-property :contents-end food-entry-headline) t)
                        ;; search succeeded
                        (progn
                          (forward-line 2)
                          (goto-char (line-beginning-position))
                          (insert "\n")
                          (forward-line -1)
                          )
                      ;; search failed
                      (progn
                          (goto-char (org-element-property :begin food-entry-headline))
                          (forward-line 1)
                          (got-char (line-beginning-position))
                          (insert food-table-head
                                  "\n"
                                  food-table-formulas)
                          (forward-line -1)
                          )
                        )
                    )
                  ;; did not find entry
                (progn
                  (goto-char (point-max))
                  (goto-char (line-beginning-position))
                  (insert "** " food-entry-raw "\n"
                          food-table-head
                          "\n"
                          food-table-formulas)
                  (forward-line -1)
                  (goto-char (line-beginning-position))
                  )
                )

              )
            ;;we are now at a blank line
            (let ((time-stamp (with-temp-buffer (org-insert-time-stamp (current-time) t t) (buffer-string)) )
                  )
              (progn
                (insert
                 (format "| # | %s | %s | %f | | | | | | | | | " time-stamp selection ammount)
                 )
                (org-table-recalculate t)
                (message "added %dg of %s" ammount selection)
                () 
                )
              )
            )
           )
         )
       )
     )
   )
  )


;; converts something like "Cheddar, cheese" to "Cheddar_cheese"
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






