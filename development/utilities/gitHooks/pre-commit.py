#!/usr/bin/python
"Git pre-commit hook. "

def commandOutput(cmd):
    import os
    return os.popen(cmd, "r").read()



class File:
    def __init__(self, sha1, filename, status):
        self.sha1 = sha1
        self.filename = filename
        self.status = status
    def __str__(self):
        return str(self.filename)
    def shouldBeChecked(self):
        import os
        return os.path.splitext(self.filename)[1] in ".cs .c .C .cpp .cxx .cc .CC .c++ .h .H .hpp .hxx .hh .HH .h++".split()
    def notThirdParty(self):
        import re
        return not re.search("/thirdParty/", self.filename)
    def hasBeenAddedModifiedCopiedOrRenamed(self):
        return not (self.status in "U D".split())
    def isUnmerged(self):
        return self.status == "U"
    def containsLeadingTabs(self):
        import re
        res = re.search("^\t+", self.fileContents(), re.M)
        return res
    def containsTrailingWhitespace(self):
        import re
        res = re.search("[ \t]+$", self.fileContents(), re.M)
        return res
    def fileContents(self):
        return commandOutput("git-cat-file blob %s" % self.sha1)



outputDelimiter = "\n    "

def checkForLeadingTabs(files):
    filesWithLeadingTabs = filter(lambda(file): file.containsLeadingTabs(), files)
    if len(filesWithLeadingTabs) > 0:
        print >> sys.stderr, "The following files contain leading tabs:%s%s\n" % (outputDelimiter, outputDelimiter.join(map(lambda(file): str(file), filesWithLeadingTabs)))
    return len(filesWithLeadingTabs)

def checkForTrailingWhitespace(files):
    filesWithTrailingWhitespace = filter(lambda(file): file.containsTrailingWhitespace(), files)
    if len(filesWithTrailingWhitespace) > 0:
        print >> sys.stderr, "The following files contain trailing whitespace:%s%s\n" % (outputDelimiter, outputDelimiter.join(map(lambda(file): str(file), filesWithTrailingWhitespace)))
    return len(filesWithTrailingWhitespace)

def checkForBeingUnmerged(files):
    unmergedFiles = filter(lambda (file): file.isUnmerged(), files)
    if len(unmergedFiles) > 0:
        print >> sys.stderr, "The following files are unmerged: %s%s\n" % (outputDelimiter, outputDelimiter.join(map(lambda(file): str(file), unmergedFiles)))
    return len(unmergedFiles)

sha1PositionInCommandOutput = 3
filenamePositionInCommandOutput = 5
statusPositionInCommandOutput = 4

def filesChanged():
    return [(lambda(pieces): File(pieces[sha1PositionInCommandOutput], pieces[filenamePositionInCommandOutput], pieces[statusPositionInCommandOutput]))(line.split())
            for line in commandOutput("git-diff-index --cached HEAD").split("\n") if len(line) > statusPositionInCommandOutput]


def checkChangedFiles():
    files = filesChanged()

    error = checkForBeingUnmerged(files)

    files = filter(lambda(file): file.hasBeenAddedModifiedCopiedOrRenamed(), files)

    files = filter(lambda(file): file.shouldBeChecked(), files)
    files = filter(lambda(file): file.notThirdParty(), files)

    error += checkForLeadingTabs(files)
    error += checkForTrailingWhitespace(files)

    return error

def main():
    errors = 0
    errors += checkChangedFiles()

    if errors > 0 :
        print >> sys.stderr, "\nPlease correct errors reported above."

    return errors

if __name__ == "__main__":
    import sys
    sys.exit(main())
