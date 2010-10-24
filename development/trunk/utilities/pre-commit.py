#!/usr/sfw/bin/python
" Example Subversion pre-commit hook. "

def command_output(cmd):
	" Capture a command's standard output. "
	import os
	return os.popen(cmd, "r").read()

def file_contents(filename, look_cmd):
	" Return a file's contents for this transaction. "
	return command_output(
		 "%s %s" % (look_cmd % "cat", filename))

def contains_trailing_whitespace(filename, look_cmd):
	" Return True if this version of the file contains trailings. "
	import re
	res = re.search("[ \t]+$", file_contents(filename, look_cmd), re.M)
	return res

output_delimiter = "\n    "

def check_trailing_spaces(look_cmd, files):
	" Check files are trailing-spaces-free. "
	file_with_trailing_whitespace = [ff for ff in files if contains_trailing_whitespace(ff, look_cmd)]
	if len(file_with_trailing_whitespace) > 0:
		print >> sys.stderr, "The following files contain trailing whitespace:%s%s\n" % (output_delimiter, output_delimiter.join(file_with_trailing_whitespace))
	return len(file_with_trailing_whitespace)

def contains_leading_tabs(filename, look_cmd):
	" Return True if this version of the file contains leading tabs. "
	import re
	res = re.search("^\t+", file_contents(filename, look_cmd), re.M) 
	return res

def check_leading_tabs(look_cmd, files):
	" Check files are tab-free. "
	files_with_leading_tabs = [ff for ff in files if contains_leading_tabs(ff, look_cmd)]
	if len(files_with_leading_tabs) > 0:
		print >> sys.stderr, "The following files contain leading tabs:%s%s\n" % (output_delimiter, output_delimiter.join(files_with_leading_tabs))
	return len(files_with_leading_tabs)

def files_changed(look_cmd):
	""" List the files added or updated by this transaction.

	"svnlook changed" gives output like:
	U	 trunk/file1.cpp
	A	 trunk/file2.cpp
	"""
	def filename(line):
		return line.split()[1]
	def added_or_updated(line):
		return line and line[0] in ("A", "U")
	return [
			filename(line)
			for line in command_output(look_cmd % "changed").split("\n")
			if added_or_updated(line)]

def check_changed_files(look_cmd):
	" Check changed files in this transaction. " 
	def is_file_that_should_be_checked(fname):
		import os
		return os.path.splitext(fname)[1] in ".boo .c .C .cpp .cxx .h .H .hpp .hxx".split()
	def not_3rdparty(fname):
		import re
		return not re.search("/thirdParty/", fname)	
	files = files_changed(look_cmd)
	files = filter(is_file_that_should_be_checked, files[:])
	files = filter(not_3rdparty, files[:])

	error = 0
	error+= check_leading_tabs(look_cmd, files)
	error+= check_trailing_spaces(look_cmd, files)
	return error

def main():
	usage = """usage: %prog REPOS TXN
	Run pre-commit options on a repository transaction."""
	from optparse import OptionParser
	parser = OptionParser(usage=usage)
	parser.add_option("-r", "--revision",
			  help="Test mode. TXN actually refers to a revision.",
			  action="store_true", default=False)
	errors = 0
	try:
		(opts, (repos, txn_or_rvn)) = parser.parse_args()
		look_opt = ("--transaction", "--revision")[opts.revision]
		look_cmd = "svnlook %s %s %s %s" % ("%s", repos, look_opt, txn_or_rvn)

		
		errors += check_changed_files(look_cmd)

		if errors > 0 :
			print >> sys.stderr, "\nPlease remove leading tabs and trailing spaces."
	except ValueError:
		print >> sys.stderr, "Unexpected error:", sys.exc_info()
		parser.print_help()
		errors += 1

	return errors

if __name__ == "__main__":
	import sys
	sys.exit(main())
