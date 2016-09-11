(defproject shogiandroid/shogiandroid "0.1.0-SNAPSHOT"
  :description "FIXME: Android project description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :global-vars {*warn-on-reflection* true}

  :source-paths ["src/clojure" "src"]
  :java-source-paths ["src/java"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :plugins [[lein-droid "0.4.3"]]

  :dependencies [[org.clojure-android/clojure "1.7.0-r2" :use-resources true]
                    [com.android.support/support-v4 "23.0.0" :extension "aar"]
                    [com.google.android.gms/play-services-auth "8.4.0" :extension "aar"]
                    [com.google.android.gms/play-services-gcm "8.4.0" :extension "aar"]
                 [neko/neko "4.0.0-alpha5"]
                 [org.clojure/tools.nrepl "0.2.10" :use-resources true]]
  :profiles {:default [:dev]

             :dev
             [:android-common :android-user
              {:dependencies [
                              [org.clojure/tools.nrepl "0.2.10"]
                              ]
               :target-path "target/debug"
               :android {:aot :all-with-unused
                         :rename-manifest-package "agendine.draft.shogiandroid.debug"
                         :manifest-options {:app-name "shogiandroid (debug)"
                                            :applicationID "agendine.draft.shogiandroid"
                                            }
                         }}]
             :release
             [:android-common
              {:target-path "target/release"
               :android
               {;; :keystore-path "/home/user/.android/private.keystore"
                ;; :key-alias "mykeyalias"
                ;; :sigalg "MD5withRSA"

                :ignore-log-priority [:debug :verbose]
                :aot :all
                :build-type :release}}]
             :android-user {:dependencies [
                                           [cider/cider-nrepl "0.10.1"]]
                            :android {:aot-exclude-ns ["cider.nrepl.middleware.util.java.parser"
                                                       "cider.nrepl" "cider-nrepl.plugin"]}}
             :android-common
             {:android
              {}
              :dependencies []}}


  :android {;; Specify the path to the Android SDK directory.
            ;; :sdk-path "/home/user/path/to/android-sdk/"

           :sdk-path "/Users/Agendine/Documents/Development/Mobile/sdk/android-sdk-macosx"
            ;; Try increasing this value if dexer fails with
            ;; OutOfMemoryException. Set the value according to your
            ;; available RAM.
            ;; :dex-opts ["-JXmx4096M" "--incremental"]
            :dex-opts ["-JXmx5120M" "--incremental"]

            :target-version "22"
            :aot-exclude-ns ["clojure.parallel" "clojure.core.reducers"
                             "cider.nrepl" "cider-nrepl.plugin"
                             "cider.nrepl.middleware.util.java.parser"
                             #"cljs-tooling\..+"]
           :dependencies [
                          ]})
