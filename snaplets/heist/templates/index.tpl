<!DOCTYPE html>
<html lang="en-US">
<html>
  <head>
    <meta charset="UTF-8">
    <link rel="stylesheet" href="css/styles.css" type="text/css">
    <title>Quick notes!</title>
    
    <!-- Internet Explorer support ... -->
    <!--[if lt IE 9]>
      <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
  </head>
  <body>  
    <header>
      <h1>Quick Notes!</h1>
      <p>Enter your notes and click 'Create PDF'</p>
    </header>
    <div id="container">
      <nav>
        <h2>Special syntax:</h2>
        <p>Escape characters with '\'</p>
        <ul id="menu">
          <li><i>##c</i>  - Text ident</li>
          <li><i>##br</i> - Line break</li>
          <li><i>##np</i> - Page break</li>
        </ul>
      </nav>
      <section>
        <form name="foo" method="post">
          <h5>Author (optional):<br>
          <input type="text" name="author" size="100" autocomplete="on" autofocus> </h5>
          <h5>Notes:<br>
          <textarea name="note" cols="100" rows="20" autocomplete="off" placeholder="Enter your notes here!" required></textarea><br>
          <input type="submit" value="Create PDF" /></h5>
        </form>
      </section>
    </div>
    <footer>
      <!-- Eventuell copyright osv -->
      <p><a href="https://github.com/joacand/QuickNote">QuickNote v0.1</a> - Written in Haskell</p>
    </footer>
  </body>
</html>

