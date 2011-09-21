(ns logjure.utils.defmultimethod-test
  (:use logjure.utils.defmultimethod clojure.contrib.test-is))

(derive ::logjure.utils.defmultimethod-test/Savings ::logjure.utils.defmultimethod-test/Account)
(derive ::logjure.utils.defmultimethod-test/Checking ::logjure.utils.defmultimethod-test/Account)

(defstruct account :id :tag :balance)
(def a1 (struct account 1 ::logjure.utils.defmultimethod-test/Checking 100M))
(def a2 (struct account 2 ::logjure.utils.defmultimethod-test/Savings 250M))
(def a3 (struct account 3 ::logjure.utils.defmultimethod-test/Savings 1001M))
(def a4 (struct account 4 ::logjure.utils.defmultimethod-test/Checking 5001M))

(defmultimethod interest-rate [acct] :tag
  ::logjure.utils.defmultimethod-test/Checking 0M
  ::logjure.utils.defmultimethod-test/Savings 0.05M)
(deftest test-interest-rate
  (is (= 0M (interest-rate a1)))
  (is (= 0.05M (interest-rate a2)))
  )

(defmultimethod account-level [acct] :tag
  ::logjure.utils.defmultimethod-test/Checking (if (>= (:balance acct) 5000) ::logjure.utils.defmultimethod-test/Premium ::logjure.utils.defmultimethod-test/Basic)
  ::logjure.utils.defmultimethod-test/Savings (if (>= (:balance acct) 1000) ::logjure.utils.defmultimethod-test/Premium ::logjure.utils.defmultimethod-test/Basic)
  )
(deftest test-account-level
  (is (= ::logjure.utils.defmultimethod-test/Basic (account-level a1)))
  (is (= ::logjure.utils.defmultimethod-test/Basic (account-level a2)))
  (is (= ::logjure.utils.defmultimethod-test/Premium (account-level a3)))
  (is (= ::logjure.utils.defmultimethod-test/Premium (account-level a4)))
  )

;test multimethod with dispatch on vector

(defmultimethod service-charge [acct] (fn [acct] [(account-level acct) (:tag acct)])
  [::logjure.utils.defmultimethod-test/Basic ::logjure.utils.defmultimethod-test/Checking] 25
  [::logjure.utils.defmultimethod-test/Basic ::logjure.utils.defmultimethod-test/Savings] 10
  [::logjure.utils.defmultimethod-test/Premium ::logjure.utils.defmultimethod-test/Account] 0
  )
(deftest test-service-charge
  (is (= 25 (service-charge a1)))
  (is (= 10 (service-charge a2)))
  (is (= 0 (service-charge a3)))
  (is (= 0 (service-charge a4)))
  )

;test multimethod with multiple arguments

(defmultimethod account-greeting [acct person] 
  (fn 
    ([acct person] [(account-level acct) (:tag acct)]))
  [::logjure.utils.defmultimethod-test/Basic ::logjure.utils.defmultimethod-test/Checking] (str "welcome " person)
  [::logjure.utils.defmultimethod-test/Basic ::logjure.utils.defmultimethod-test/Savings] (str "welcome dear " person)
  [::logjure.utils.defmultimethod-test/Premium ::logjure.utils.defmultimethod-test/Account] (str "welcome master " person)
  )
(deftest test-account-greeting
  (is (= "welcome :piotr" (account-greeting a1 :piotr)))
  (is (= "welcome dear :piotr" (account-greeting a2 :piotr)))
  (is (= "welcome master :piotr" (account-greeting a3 :piotr)))
  (is (= "welcome master :piotr" (account-greeting a4 :piotr)))
  )

;test defmultimethod with different arity

(defn argvect [f1 & fs] (vec (map class (if fs (cons f1 fs) (list f1)))))
(deftest test-argvect 
  (is (= [String] (argvect "s")))
  (is (= [String String] (argvect "s1" "s2")))
  )

(defmultimethod fx [f1 & fs] argvect
  [java.io.File] f1
  [String] (java.io.File. f1)
  [String String] (list (fx f1) (fx (first fs)))
  )
(deftest test-fx 
  (is (= java.io.File (class  (fx "c:\\"))))
  (is (= java.io.File (class  (fx (java.io.File. "c:\\")))))
  (is (= (list (java.io.File. "c:\\") (java.io.File. "c:\\")) (fx "c:\\" "c:\\")))
  )

;test defmultimethod with different arity & fn as dispatch value
;TODO..

(run-tests)
