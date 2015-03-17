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
      <h1>Quick notes!</h1>
      <p>Enter your notes, with the optional author and press submit.</p>
    </header>
    <div id="container">
      <nav>
        <h2>Special syntax:</h2>
        <ul id="menu">
          <li><i>#c#</i> - For indented and unparsed text, good for code</li>
          <li><i>#n#</i> - Force newline</li>
        </ul>
      </nav>
      <section>
        <form name="foo" method="post">
          <h5>Author (optional):<br>
          <input type="text" name="author" size="100" autocomplete="on" autofocus> </h5>
          <h5>Note:<br>
          <textarea name="note" cols="100" rows="20" autocomplete="off" placeholder="Enter your notes here!" required></textarea><br>
          <input type="submit" value="Submit" /></h5>
        </form>
      </section>
    </div>
    <footer>
      <!-- Eventuell copyright osv -->
      <p>QuickNote v0.1 - Written in Haskell</p>
    </footer>
  </body>
</html>

