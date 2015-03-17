# QuickNote
Web program for quickly taking notes and coverting them to a pretty LaTeX PDF document. Built in Haskell using the Snap web framework.

The program 'pdflatex' from the TeX Live package is required for generating the PDF.

How to install using cabal:
cabal init sandbox -- Sandbox is recommended
cabal configure
cabal install -- This might take a while
dist/.../build/QuickNote/QuickNote -p 8000

You can reach it at URL http://localhost:8000/
