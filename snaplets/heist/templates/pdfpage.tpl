<apply template="base">
<p><a href="static/note.pdf">Click here</a> to download the PDF</p>

<form>
  <input id="backbtn" type="button" class="btn btn-blue" value="Back" onClick="history.go(-1);return true;"/>
</form>
<br>

<object data="static/note.pdf" type="application/pdf" width="100%" height="100%">

  <p>It appears you don't have a PDF plugin for this browser.
  No biggie... you can <a href="static/note.pdf">click here to
  download the PDF file.</a></p>

</object>

</apply>
