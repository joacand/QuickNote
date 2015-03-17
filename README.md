# QuickNote
Web program for quickly taking notes and coverting them to a pretty LaTeX PDF document. Built in Haskell using the Snap web framework.

The program 'pdflatex' from the TeX Live package is required for generating the PDF.

How to install using cabal: <br />
cabal init sandbox -- Sandbox is recommended <br />
cabal configure <br />
cabal install -- This might take a while <br />
dist/.../build/QuickNote/QuickNote -p 8000 <br />

You can reach it at URL http://localhost:8000/
