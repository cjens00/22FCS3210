import os
import sys


def do_it():
    templateString1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" "\n" \
                      "<module type=\"JAVA_MODULE\" version=\"4\">" "\n" \
                      "  <component name=\"NewModuleRootManager\" inherit-compiler-output=\"true\">" "\n" \
                      "      <exclude-output />" "\n" \
                      "      <content url=\"file://$MODULE_DIR$\">" "\n" \
                      "          <sourceFolder url=\"file://$MODULE_DIR$/"
    templateString2 = "\" isTestSource=\"false\" />" "\n" \
                      "      </content>" "\n" \
                      "      <orderEntry type=\"inheritedJdk\" />" "\n" \
                      "      <orderEntry type=\"library\" exported=\"\" " \
                      "name=\"scala-sdk-2.13.10\" level=\"application\" />" "\n" \
                      "      <orderEntry type=\"sourceFolder\" forTests=\"false\" />" "\n" \
                      "  </component>" "\n" \
                      "</module>"
    for dir in os.listdir("./"):
        dirName = str(dir)
        if "." not in dirName:
            with open(f"{dirName}/{dirName}.iml", "w") as outFile:
                print(f"Writing new file {dirName}/{dirName}.iml")
                outFile.write(templateString1)
                outFile.write(dirName)
                outFile.write(templateString2)


if __name__ == "__main__":
    do_it()
