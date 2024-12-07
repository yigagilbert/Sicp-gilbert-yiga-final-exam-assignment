#lang racket

(require data-science-master
         plot
         srfi/19)

;; Define TweetData struct
(struct TweetData (text location date) #:transparent)

;; Convert a date string to a Racket date object
(define (convert-timestamp str)
  (with-handlers ([exn:fail? 
                   (λ (exn)
                     (displayln (format "Invalid date format: ~a" str))
                     #f)])
    (string->date str "~Y-~m-~d")))

;; Convert raw tweet data to a list of TweetData structs
(define (parse-tweets raw-tweets)
  (map (λ (tweet)
         (TweetData (list-ref tweet 0)
                    (list-ref tweet 1)
                    (convert-timestamp (list-ref tweet 2))))
       raw-tweets))

;; Filter tweets by country
(define (extract-country-tweets tweets country)
  (filter (λ (tweet) (string=? (TweetData-location tweet) country)) tweets))

;; Group tweets by month
(define (group-tweets-by-month tweets)
  (let ([grouped (make-hash)])
    (for-each
     (λ (tweet)
       (let* ([month (date->string (TweetData-date tweet) "~Y-~m")]
              [existing (hash-ref grouped month '())])
         (hash-set! grouped month (cons tweet existing))))
     tweets)
    grouped))

;; Define SentimentAnalyzer struct
(struct SentimentAnalyzer (lexicon) #:transparent)

;; Factory function to create a SentimentAnalyzer
(define (make-sentiment-analyzer [lexicon 'nrc])
  (SentimentAnalyzer lexicon))

;; Tokenize tweets and calculate word frequencies
(define (tokenize-and-count tweets)
  (if (null? tweets)
      '()
      (document->tokens
       (string-join (map TweetData-text tweets) " ")
       #:sort? #t)))

;; Analyze sentiments using lexicon
(define (analyze-sentiments analyzer tokens)
  (filter
   (λ (entry)
     (and (list? entry)
          (= (length entry) 3)
          (number? (list-ref entry 2))))
   (list->sentiment tokens #:lexicon (SentimentAnalyzer-lexicon analyzer))))

;; Aggregate sentiment frequencies
(define (aggregate-sentiments sentiments)
  (let ([aggregated (make-hash)])
    (for-each
     (λ (sentiment)
       (let* ([sentiment-type (list-ref sentiment 1)]
              [freq (list-ref sentiment 2)]
              [existing (hash-ref aggregated sentiment-type 0)])
         (hash-set! aggregated sentiment-type (+ existing freq))))
     sentiments)
    aggregated))

;; Define Visualizer struct
(struct Visualizer () #:transparent)

;; Factory function to create a Visualizer
(define (make-visualizer)
  (Visualizer))

;; Generate a list of colors for each month
(define month-colors
  '("Red" "Orange" "Yellow" "Green" "Blue" "Indigo" "Violet" 
    "Brown" "Pink" "Cyan" "Magenta" "Gray"))

;; Plot sentiments by month with unique colors
(define (plot-sentiments-by-month visualizer sentiments-by-month)
  (if (null? (hash-keys sentiments-by-month))
      (displayln "No data to visualize.")
      (parameterize ([plot-width 800]
                     [plot-height 600]
                     [plot-x-label "Sentiment"]
                     [plot-y-label "Frequency"])
        (plot
         (list
          (tick-grid)
          (for/list ([month (sort (hash-keys sentiments-by-month) string<?)]
                     [color month-colors]) ;; Assign a unique color to each month
            (let* ([month-sentiments (hash-ref sentiments-by-month month)]
                   [data (hash->list month-sentiments)])
              (discrete-histogram
               (map (λ (pair) (list (car pair) (cdr pair))) data)
               #:color color
               #:line-color color
               #:label month))))))))

;; Plot sentiments for a single month
(define (plot-sentiments-for-month visualizer month sentiments-by-month)
  (let ([month-sentiments (hash-ref sentiments-by-month month '())])
    (if (null? month-sentiments)
        (displayln (format "No data available for ~a." month))
        (parameterize ([plot-width 800]
                       [plot-height 600]
                       [plot-x-label "Sentiment"]
                       [plot-y-label "Frequency"])
          (plot
           (list
            (tick-grid)
            (discrete-histogram
             (map (λ (pair) (list (car pair) (cdr pair)))
                  (hash->list month-sentiments))
             #:color "Blue"
             #:line-color "Blue"
             #:label month)))))))


;; Define TweetMoodAnalyzer struct at the top level
(struct TweetMoodAnalyzer (analyzer visualizer) #:transparent)

;; Factory function to create a TweetMoodAnalyzer
(define (make-tweet-mood-analyzer analyzer visualizer)
  (TweetMoodAnalyzer analyzer visualizer))

;; Analyze moods for a country over 12 months
(define (analyze-country-moods analyzer country raw-tweets)
  (let* ([tweets (parse-tweets raw-tweets)]
         [country-tweets (extract-country-tweets tweets country)]
         [grouped-tweets (group-tweets-by-month country-tweets)]
         [sentiments-by-month
          (for/hash ([month (hash-keys grouped-tweets)])
            (values month
                    (aggregate-sentiments
                     (analyze-sentiments (TweetMoodAnalyzer-analyzer analyzer)
                                         (tokenize-and-count
                                          (hash-ref grouped-tweets month))))))])
    sentiments-by-month))


;; Visualize country moods and print results
(define (visualize-country-moods analyzer country raw-tweets)
  (let ([sentiments-by-month (analyze-country-moods analyzer country raw-tweets)])
    ;; Print results before plotting
    (displayln "Sentiment Analysis Results by Month:")
    (for ([month (sort (hash-keys sentiments-by-month) string<?)])
      (let ([month-sentiments (hash-ref sentiments-by-month month)])
        (printf "Month: ~a\n" month)
        (for ([sentiment (hash->list month-sentiments)])
          (printf "  Sentiment: ~a, Frequency: ~a\n" (car sentiment) (cdr sentiment)))
        (newline))) ;; Add space between months
    ;; Proceed to plotting
    (plot-sentiments-by-month (TweetMoodAnalyzer-visualizer analyzer) sentiments-by-month)))


;; Sample tweets
(define raw-tweets
  '(
    ;; January
    ("Happy to be in Uganda!" "Uganda" "2024-01-10")
    ("Traffic is so bad today, it's frustrating." "Uganda" "2024-01-15")
    ("Enjoying a peaceful morning in Kampala." "Uganda" "2024-01-20")
    ("Worried about the economy." "Uganda" "2024-01-25")
    ("Great coffee at my favorite cafe!" "Uganda" "2024-01-30")
    ("Agricultural challenges are real this season." "Uganda" "2024-01-05")
    ("Beautiful sunset over Lake Victoria." "Uganda" "2024-01-12")
    ("Supporting local businesses is crucial." "Uganda" "2024-01-18")
    ("Education reforms need more attention." "Uganda" "2024-01-22")
    ("Loving the local street food scene!" "Uganda" "2024-01-28")

    ;; February
    ("The weather is beautiful today!" "Uganda" "2024-02-05")
    ("Feeling grateful for my friends and family." "Uganda" "2024-02-10")
    ("Another boring day in traffic." "Uganda" "2024-02-15")
    ("Proud to be Ugandan this February!" "Uganda" "2024-02-20")
    ("Excited to watch the national football match!" "Uganda" "2024-02-25")
    ("Cultural diversity is our strength." "Uganda" "2024-02-02")
    ("Supporting women's entrepreneurship." "Uganda" "2024-02-07")
    ("Environmental conservation matters." "Uganda" "2024-02-13")
    ("Tech innovation in Kampala is inspiring." "Uganda" "2024-02-17")
    ("Community spirit keeps us strong." "Uganda" "2024-02-23")

    ;; March
    ("March mornings are so refreshing." "Uganda" "2024-03-01")
    ("Disappointed with the new government policy." "Uganda" "2024-03-05")
    ("Traffic jams are unbearable as always." "Uganda" "2024-03-10")
    ("Enjoying some time with nature." "Uganda" "2024-03-15")
    ("Feeling optimistic about the future." "Uganda" "2024-03-20")
    ("Agricultural harvest looking promising." "Uganda" "2024-03-03")
    ("Youth leadership programs are crucial." "Uganda" "2024-03-08")
    ("Infrastructure development is key." "Uganda" "2024-03-12")
    ("Mental health awareness growing." "Uganda" "2024-03-17")
    ("Local art scene is vibrant!" "Uganda" "2024-03-22")

    ;; April
    ("Sad news in the papers today." "Uganda" "2024-04-01")
    ("The city is alive with energy this April." "Uganda" "2024-04-10")
    ("Frustrated by the rising cost of living." "Uganda" "2024-04-15")
    ("Feeling motivated to start new projects." "Uganda" "2024-04-20")
    ("Happy to spend the weekend with family." "Uganda" "2024-04-25")
    ("Educational opportunities expanding." "Uganda" "2024-04-03")
    ("Supporting renewable energy initiatives." "Uganda" "2024-04-07")
    ("Tourism potential is incredible." "Uganda" "2024-04-13")
    ("Celebrating local entrepreneurs." "Uganda" "2024-04-18")
    ("Community resilience is inspiring." "Uganda" "2024-04-23")

    ;; May
    ("Beautiful sunshine in Kampala this morning." "Uganda" "2024-05-05")
    ("Excited for the long weekend ahead!" "Uganda" "2024-05-10")
    ("Sad about the recent floods in the north." "Uganda" "2024-05-15")
    ("Great meeting with my colleagues today." "Uganda" "2024-05-20")
    ("Proud of our achievements this month!" "Uganda" "2024-05-25")
    ("Agricultural innovations making progress." "Uganda" "2024-05-02")
    ("Cultural exchange programs rock!" "Uganda" "2024-05-07")
    ("Healthcare improvements are visible." "Uganda" "2024-05-12")
    ("Youth employment challenges persist." "Uganda" "2024-05-17")
    ("Local cuisine is a national treasure." "Uganda" "2024-05-22")

    ;; June
    ("Feeling happy and content this June." "Uganda" "2024-06-01")
    ("Terrible day at work, feeling stressed." "Uganda" "2024-06-05")
    ("A beautiful evening walk by the lake." "Uganda" "2024-06-10")
    ("Worried about the ongoing drought." "Uganda" "2024-06-15")
    ("June is such a wonderful month." "Uganda" "2024-06-20")
    ("Sports development needs investment." "Uganda" "2024-06-03")
    ("Environmental education is crucial." "Uganda" "2024-06-08")
    ("Tech startups are game-changers." "Uganda" "2024-06-13")
    ("Community health initiatives expanding." "Uganda" "2024-06-17")
    ("Celebrating local music and arts." "Uganda" "2024-06-22")

    ;; July
    ("Celebrating our culture this July!" "Uganda" "2024-07-01")
    ("The new policy is a disaster." "Uganda" "2024-07-05")
    ("Feeling hopeful about our community projects." "Uganda" "2024-07-10")
    ("Proud of the team for the great work!" "Uganda" "2024-07-15")
    ("Another beautiful sunset in Kampala." "Uganda" "2024-07-20")
    ("Agricultural research making strides." "Uganda" "2024-07-03")
    ("Women's empowerment is progressing." "Uganda" "2024-07-08")
    ("Educational reforms taking shape." "Uganda" "2024-07-12")
    ("Local innovation ecosystem growing." "Uganda" "2024-07-17")
    ("Cultural diversity celebration!" "Uganda" "2024-07-22")

    ;; August
    ("Worried about the upcoming elections." "Uganda" "2024-08-01")
    ("Feeling energetic and motivated this August." "Uganda" "2024-08-05")
    ("Traffic is still terrible." "Uganda" "2024-08-10")
    ("Excited for the national holiday celebrations." "Uganda" "2024-08-15")
    ("Uganda is such a beautiful country!" "Uganda" "2024-08-20")
    ("Agricultural sustainability matters." "Uganda" "2024-08-03")
    ("Youth leadership conference inspiring." "Uganda" "2024-08-08")
    ("Infrastructure challenges continue." "Uganda" "2024-08-12")
    ("Tech innovation ecosystem thriving." "Uganda" "2024-08-17")
    ("Community resilience is remarkable." "Uganda" "2024-08-22")

    ;; September
    ("Excited for the elections this month!" "Uganda" "2024-09-01")
    ("Worried about the state of the nation." "Uganda" "2024-09-05")
    ("Feeling proud of our progress so far." "Uganda" "2024-09-10")
    ("Another lovely day in Kampala." "Uganda" "2024-09-15")
    ("Happy to be alive and healthy!" "Uganda" "2024-09-20")
    ("Agricultural policy needs review." "Uganda" "2024-09-03")
    ("Educational opportunities expanding." "Uganda" "2024-09-08")
    ("Local entrepreneurship is key." "Uganda" "2024-09-12")
    ("Climate change adaptation crucial." "Uganda" "2024-09-17")
    ("Cultural heritage preservation important." "Uganda" "2024-09-22")

    ;; October
    ("October evenings are so peaceful." "Uganda" "2024-10-01")
    ("Feeling sad about the global news." "Uganda" "2024-10-05")
    ("Grateful for my supportive community." "Uganda" "2024-10-10")
    ("Excited to attend the cultural festival!" "Uganda" "2024-10-15")
    ("The weather is perfect this October." "Uganda" "2024-10-20")
    ("Agricultural innovations showcased." "Uganda" "2024-10-03")
    ("Youth engagement in politics growing." "Uganda" "2024-10-08")
    ("Healthcare improvements continuing." "Uganda" "2024-10-12")
    ("Tech ecosystem attracting investment." "Uganda" "2024-10-17")
    ("Community development initiatives strong." "Uganda" "2024-10-22")

    ;; November
    ("Starting the month with optimism." "Uganda" "2024-11-01")
    ("Worried about the upcoming budget decisions." "Uganda" "2024-11-05")
    ("Excited to join the new initiative." "Uganda" "2024-11-10")
    ("Happy to see progress on our projects." "Uganda" "2024-11-15")
    ("November has been a busy but rewarding month." "Uganda" "2024-11-20")
    ("Agricultural research breakthrough." "Uganda" "2024-11-03")
    ("Women in leadership making impact." "Uganda" "2024-11-08")
    ("Educational reform discussions ongoing." "Uganda" "2024-11-12")
    ("Local innovation ecosystem expanding." "Uganda" "2024-11-17")
    ("Cultural exchange programs successful." "Uganda" "2024-11-22")

    ;; December
    ("Celebrating the holiday season with joy!" "Uganda" "2024-12-01")
    ("Reflecting on the year's challenges." "Uganda" "2024-12-05")
    ("Happy to see friends and family again." "Uganda" "2024-12-10")
    ("Feeling hopeful for the new year." "Uganda" "2024-12-15")
    ("Thankful for all the blessings this year." "Uganda" "2024-12-20")
    ("Agricultural year in review." "Uganda" "2024-12-03")
    ("Youth achievements celebrated." "Uganda" "2024-12-08")
    ("Community resilience highlighted." "Uganda" "2024-12-12")
    ("Technological advancements impressive." "Uganda" "2024-12-17")
    ("Cultural unity stronger than ever." "Uganda" "2024-12-22")
  ))


;; Initialize the system
(define analyzer
  (make-tweet-mood-analyzer (make-sentiment-analyzer 'nrc)
                            (make-visualizer)))

;; Analyze and visualize
(visualize-country-moods analyzer "Uganda" raw-tweets)

;; Drill down into a specific month
(define sentiments-by-month (analyze-country-moods analyzer "Uganda" raw-tweets))
(plot-sentiments-for-month (make-visualizer) "2024-01" sentiments-by-month)

