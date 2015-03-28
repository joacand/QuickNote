<apply template="base">
    <header>
      <h1>Quick Notes!</h1>
      <p>Enter your notes and click 'Create PDF'</p>
    </header>
    <div id="container">
      <nav>
        <h2>Instructions</h2>
        <p>Escape characters with '}'.</p>
        <p>You can write raw LaTeX code with the regular \ LaTeX syntax. Make sure any braces, commands or environments are properly closed.</p>
        <h3>Special syntax:</h3>
        <ul id="menu">
          <li><i>##c</i>  - Text ident</li>
          <li><i>##br</i> - Line break</li>
          <li><i>##np</i> - Page break</li>
        </ul>
        <h5><error/></h5>
      </nav>
      <section>
        <form name="foo" method="post">
          <div id="wrapper">
            <div id="first">
              <h5>Title:<br>
              <input type="text" name="title" size="72" autocomplete="on" autofocus> </h5>
            </div>
            <div id="second">
              <h5>Author:<br>
              <input type="text" name="author" size="73" autocomplete="on"> </h5>
            </div>
          </div>
          <h5>
          <textarea name="note" cols="150" rows="20" autocomplete="off" placeholder="Enter your notes here!" required><oldnotes/></textarea><br>
          <input type="submit" value="Create PDF" class="btn btn-blue" /></h5>
        </form>
      </section>
    </div>
    <footer>
      <!-- Eventuell copyright osv -->
      <p><a href="https://github.com/joacand/QuickNote">QuickNote v0.1</a> - Written in Haskell</p>
    </footer>
</apply>

