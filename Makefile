
test:
	csi -s tests/run.scm
	csi -s tests/examples.scm
	csi -s tests/tga.scm
	csc tests/examples.scm
	tests/examples
	csc tests/tga.scm
	tests/tga

