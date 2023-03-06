(import
 datetime :as dt
 functools [partial]
 glob
 html
 string
 matplotlib.pyplot :as plt
 numpy :as np
 pandas :as pd
 seaborn :as sns
 sklearn.compose [ColumnTransformer]
 sklearn.feature-selection [SelectPercentile chi2]
 sklearn.linear-model [ElasticNet Lasso LinearRegression Ridge]
 sklearn.metrics [r2-score]
 sklearn.model-selection [cross-validate train-test-split]
 sklearn.pipeline [make-pipeline]
 sklearn.preprocessing [OneHotEncoder StandardScaler]
 sklearn.svm [SVC])
(require hyrule :macros [ncut -> as-> doto] :readers [%])

(defclass IdDict [dict]
  (defn __missing__ [self key] key))

(setv
 italic-trans (str.maketrans (dict (zip "𝘢𝘣𝘤𝘥𝘦𝘧𝘨𝘩𝘪𝘫𝘬𝘭𝘮𝘯𝘰𝘱𝘲𝘳𝘴𝘵𝘶𝘷𝘸𝘹𝘺𝘻𝘈𝘉𝘊𝘋𝘌𝘍𝘎𝘏𝘐𝘑𝘒𝘓𝘔𝘕𝘖𝘗𝘘𝘙𝘚𝘛𝘜𝘝𝘞𝘟𝘠𝘡" string.ascii-letters)))

 exclude #{1607467122279743488 ; Saying hi
           1614017926461378562 ; Typo spotted
           1615116644266680320 ; Reply to a reply
           1616384252349747200 ; Warwick LoL
           1616861949899276291 ; New PFP
           1617530870554046466 ; Retrospring announcement
           1617587491141480448 ; Retrospring announcement
           1617593643971248142 ; Retrospring question
           1618271139360567301 ; Say something vorny meme
           1618271255865720838 ; Say something vorny meme
           1618734681192820741 ; Saying thanks
           1618734683864567810 ; Raffle thoughts
           1618940118106243073 ; Word cloud thoughts
           1619268478992797698 ; Birthday 
           1619268480565645312 ; Birthday
           1620580634531078147 ; Vore switch thoughts
           1621482513347510277 ; Raffle
           1621482515272704000 ; Raffle
           1621482518896574464 ; Raffle
           1621482522344398849 ; Raffle
           1621482529642487808 ; Raffle
           1621482532867817473 ; Raffle
           1622683466847473666 ; Profile play
           1622685273665507328 ; Profile play
           1623051057310867459 ; Profile play
           1623461205783224324 ; Profile play
           1623842319706226689 ; Raffle reminder
           1624123969644134426 ; Raffle winner
           1624125020841574414 ; Raffle winner
           1624125046795927590 ; Raffle winner
           1624128690048081921 ; Profile play
           1624128844507541504 ; Profile play
           1624382970919264261 ; Profile play
           1624431526778732548 ; Profile play
           1624727617789214720 ; Profile play
           1625946131237638166 ; Profile play
           1626694622901686272 ; Profile play
           1627837090728755200} ; Reply to a reply

 root-tweet (IdDict {1607525971237765124 1607525969694445569
                     1607525974328983553 1607525969694445569
                     1607738194308780033 1607738194308780033
                     1607738195768123392 1607738194308780033
                     1607901059435098114 1607900833248645120
                     1607901248921169920 1607900833248645120
                     1608087000938315776 1608086999189028866
                     1608087003626864640 1608086999189028866
                     1608457929002455043 1608457927123435520
                     1608579410726354944 1608579409195470848
                     1609707073461256198 1609707067375329286
                     1610077877785210882 1610044280197750784
                     1615070135366356993 1615069575548370944
                     1615828025047752718 1615828023428751383
                     1616015737381916673 1616015736085831681
                     1616015739554598912 1616015736085831681
                     1616015741832073217 1616015736085831681
                     1616015744222756864 1616015736085831681
                     1617633357080440832 1617633354479980544
                     1617633359198556160 1617633354479980544
                     1617633361463508992 1617633354479980544
                     1617633363455799296 1617633354479980544
                     1617633365599088640 1617633354479980544
                     1617633367683657728 1617633354479980544
                     1618770801087631361 1618770664823087104
                     1619828819791331333 1619828537267228673
                     1619853572233113602 1619853570437947392
                     1619853575097847808 1619853570437947392
                     1619853580231671808 1619853570437947392
                     1619853582844715009 1619853570437947392
                     1619853585688432643 1619853570437947392
                     1619853588850958336 1619853570437947392
                     1619853591786954752 1619853570437947392
                     1619853595322781697 1619853570437947392
                     1619853599131209729 1619853570437947392
                     1619853602146902017 1619853570437947392
                     1619853605519134720 1619853570437947392
                     1622033502383345664 1622032608266752001
                     1622353218595348480 1622353216909221889
                     1625611157498109956 1625611155837165696
                     1627411549135544320 1627411547843620867
                     1628879507246514180 1628879505803575298})

 tweet-data (as->
             (glob.glob "data/tweet_activity_metrics_IbzanIsHungry_*_en.csv") it
              ; read in all tweets
              (map pd.read-csv it)
              (pd.concat it :ignore-index True)
              ; clean up column names
              (.rename it :columns str.lower)
              (.rename it :columns #%(.replace %1 " " "_"))
              ; remove tweets that are replies to other people
              (ncut it.loc (-> it.tweet-text (.str.startswith "@") bnot))
              ; sort by tweet id so that tweets will be in chronological order
              ; threads are recorded as the same time so id is used to disambiguate
              (.sort-values it :by "tweet_id")
              ; remove excluded tweets
              (ncut it.loc (-> it.tweet-id (.isin exclude) bnot))
              (.assign it
                       ; parse times as times
                       :time (pd.to-datetime it.time :format "%Y-%m-%d %H:%M %z")
                       ; replace italic text with regular ASCII and add trailing newlines, 
                       ; and unescape HTML encodings
                       :tweet-text (-> it.tweet-text (.str.translate italic-trans) (+ "\n\n") (html.unescape))
                       ; assign root id i.e. first tweet in the thread
                       :root-tweet (.map it.tweet-id root-tweet)))

 to-tag (as-> tweet-data it
          (.groupby it "root_tweet")
          (.agg it {"time" "min"
                    "tweet_id" "count"
                    "tweet_text" "sum"
                    "likes" "first"})
          (.rename it :columns {"tweet_id" "thread_length"
                                "tweet_text" "thread_text"})
          (.assign it :thread-text (.str.strip it.thread-text))
          (.assign it :clean-text (->
                                   (.str.replace it.thread-text "[^a-zA-Z \\n]" "" :regex True)
                                   (.str.lower)))))

(.to-csv to-tag "data/to-tag.csv")

(setv
 features
 (as-> to-tag it
   (.assign it
            :n-words (.apply it.clean-text len)
            :account-age (-> it.time
                             (- (pd.Timestamp "2022-12-26" :tz "UTC"))
                             (.dt.floor "D")
                             (. dt days))
            :morning (it.time.dt.time.between (dt.time 6) (dt.time 12) :inclusive "left")
            :afternoon (it.time.dt.time.between (dt.time 12) (dt.time 18) :inclusive "left")
            :evening (it.time.dt.time.between (dt.time 18) (dt.time 21) :inclusive "left")
            :night (| (>= it.time.dt.time (dt.time 21)) (< it.time.dt.time (dt.time 6)))
            :blood (it.clean-text.str.contains "blood")
            :bone (| #* (map it.clean-text.str.contains ["bone" "crack" "crunch" "crackle" "break"]))
            :pet (it.clean-text.str.contains "pet")
            :reform (it.clean-text.str.contains "reform")
            :bully (it.clean-text.str.contains "bully")
            :perma (| #* (map it.clean-text.str.contains ["for good" "perma" "forever" "eternity"]))
            :maw (it.clean-text.str.contains "maw")) 
   )
 X (.drop features :columns ["time" "thread_text" "clean_text" "likes"])
 y (get features "likes")
 [X-train X-test y-train y-test] (train-test-split X y :test-size 0.2 :random-state 621)
 estimators [LinearRegression Ridge Lasso ElasticNet])

(defn evaluate-estimator [cls]
  (setv estimator (make-pipeline (StandardScaler) (cls)))
  (.fit estimator X-train y-train)
  (setv r2 (.score estimator X-test y-test))
  (print f"R² for {cls.__name__}: {r2 :.2%}")
  (pd.Series :index X.columns :data (. estimator steps [-1] [1] coef-) :name cls.__name__))

(-> (map evaluate-estimator estimators)
    (pd.concat :axis 1))
