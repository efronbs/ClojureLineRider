Code goes in linerider/src/linerider
namespace should be linerider.<name>, inside of the file <name> or else leiningen won't be able to find the file without adding it to the classpath, which I don't want to do. linerider.core (core.clj) is the main file, so use that as the driver.
Add any dependencies to linerider/project.clj. This is essentially our buildscript