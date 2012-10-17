# throwdown

rescue data from docbook

    lein run outputdir srcdir/*.xml
    # or
    # create standalone jar, will be target/throwdown-version-standalone.jar
    lein uberjar
    java -jar throwdown-VERSION-standalone.jar outputdir srcdir/*.xml

standalone jar in github downloads tab
