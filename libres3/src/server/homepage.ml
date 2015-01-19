(* Include libres3.html *)
let root = 
  "\
  <!DOCTYPE html>\n\
  <html>\n\
  <head>\n\
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n\
  <title>Skylable - LibreS3 Node</title>\n\
  <meta name=\"Description\" content=\"SX Node\">\n\
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />\n\
  \n\
  <style type=\"text/css\">\n\
  \n\
  \        /*IPHONE STYLES*/\n\
  \        @media only screen and (max-width: 1110px) {\n\
  \n\
  \        .see {\n\
  \        position:static!important;\n\
  \        text-align: right;\n\
  \        margin-left:4px;\n\
  \       }  \n\
  \n\
  \        }\n\
  \n\
  \n\
  \        /*IPHONE STYLES*/\n\
  \        @media only screen and (max-width: 920px) {\n\
  \         body {\n\
  \            display: inline!important;\n\
  \         }\n\
  \n\
  \         .main {\n\
  \          width: 99%!important;\n\
  \          text-align: center;\n\
  \          margin: auto;\n\
  \         }\n\
  \n\
  \         .pp {\n\
  \            width: 91%!important;\n\
  \         }\n\
  \n\
  \         .pp p {\n\
  \            width: 98%!important;\n\
  \         }\n\
  \n\
  \        }\n\
  \        @media only screen and (max-width: 480px) {\n\
  \         body {\n\
  \            display: inline!important;\n\
  \         }\n\
  \n\
  \         .main {\n\
  \          width: 99%!important;\n\
  \          text-align: center;\n\
  \          margin: auto;\n\
  \         }\n\
  \n\
  \         .pp {\n\
  \            width: 91%!important;\n\
  \         }\n\
  \n\
  \         .pp p {\n\
  \            width: 98%!important;\n\
  \         }\n\
  \n\
  \        }\n\
  \        @media only screen and (max-width: 380px) {\n\
  \         body {\n\
  \            display: inline!important;\n\
  \         }            \n\
  \         .main {\n\
  \          width: 99%!important;\n\
  \          text-align: center;\n\
  \          margin: auto;\n\
  \         }\n\
  \         .pp {\n\
  \            width: 91%!important;\n\
  \         }    \n\
  \n\
  \         .pp p {\n\
  \            width: 98%!important;\n\
  \         }    \n\
  \     }\n\
  \n\
  body, html {\n\
  \    margin: 0;\n\
  \    padding: 0;\n\
  }\n\
  \n\
  body {\n\
  \    height: 100%;\n\
  \    -moz-background-size: cover;\n\
  \    -webkit-background-size: cover;\n\
  \    -o-background-size: cover;\n\
  \    background-size: cover;\n\
  \    display: table;\n\
  \    margin:auto;\n\
  \    font-family: \"Tahoma\", sans-serif;\n\
  \    font-weight: 200;\n\
  \    vertical-align: middle;\n\
  }\n\
  \n\
  a {\n\
  \    color: #0077c1;\n\
  \    text-decoration: none;\n\
  }\n\
  \n\
  .main {\n\
  \    width:800px;\n\
  \    height:400px;\n\
  \    margin-top: 20%;\n\
  \    margin-bottom: 20%;\n\
  \    text-align: center;\n\
  \    background: rgba(255,255,255,0.7);\n\
  \    -webkit-box-shadow: 0px 0px 14px 0px rgba(50, 50, 50, 0.8);\n\
  \    -moz-box-shadow:    0px 0px 14px 0px rgba(50, 50, 50, 0.8);\n\
  \    box-shadow:         0px 0px 14px 0px rgba(50, 50, 50, 0.8);\n\
  }\n\
  \n\
  .main img {\n\
  \    margin-top: 40px;\n\
  }\n\
  \n\
  h1 {\n\
  \    font-size: 42px;\n\
  \    font-weight: 400;\n\
  }\n\
  \n\
  .blue {\n\
  \    color: #0077c1;\n\
  }\n\
  \n\
  .pp {\n\
  \    background: #fff;\n\
  \    margin-top: 30px;\n\
  \    margin-left: auto;\n\
  \    margin-right: auto;\n\
  \    margin-bottom: 40px;\n\
  \    width:70%;\n\
  \    padding: 5px;\n\
  \    -webkit-box-shadow: 0px 0px 14px 0px rgba(50, 50, 50, 0.8);\n\
  \    -moz-box-shadow:    0px 0px 14px 0px rgba(50, 50, 50, 0.8);\n\
  \    box-shadow:         0px 0px 14px 0px rgba(50, 50, 50, 0.8);}\n\
  \n\
  p {\n\
  \    font-size: 16px;\n\
  \    border-bottom: 1px solid #cfcfcf;\n\
  \    width: 90%;\n\
  \    margin: 5px auto;\n\
  \    padding-bottom: 5px;\n\
  }\n\
  \n\
  p span {\n\
  \    color: #0077c1;\n\
  \    font-size: 24px;\n\
  }\n\
  \n\
  .learn {\n\
  \    border-radius: 10px;\n\
  \    background: #3096d6; /* Old browsers */\n\
  \    background: -moz-linear-gradient(top,  #3096d6 0%, #1f72c0 100%); /* FF3.6+ */\n\
  \    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#3096d6), color-stop(100%,#1f72c0)); /* Chrome,Safari4+ */\n\
  \    background: -webkit-linear-gradient(top,  #3096d6 0%,#1f72c0 100%); /* Chrome10+,Safari5.1+ */\n\
  \    background: -o-linear-gradient(top,  #3096d6 0%,#1f72c0 100%); /* Opera 11.10+ */\n\
  \    background: -ms-linear-gradient(top,  #3096d6 0%,#1f72c0 100%); /* IE10+ */\n\
  \    background: linear-gradient(to bottom,  #3096d6 0%,#1f72c0 100%); /* W3C */\n\
  \    filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#3096d6', endColorstr='#1f72c0',GradientType=0 ); /* IE6-9 */\n\
  \    padding-top: 15px;\n\
  \    padding-bottom: 15px;\n\
  \    padding-left: 20px;\n\
  \    padding-right: 20px;\n\
  \    color: #fff;\n\
  \    text-decoration: none;\n\
  \    font-family: 'Arial', sans-serif;\n\
  \    margin-top: 110px;\n\
  }\n\
  \n\
  .learn:hover {\n\
  \    background: #1f72c0; /* Old browsers */\n\
  \    background: -moz-linear-gradient(top, #1f72c0 0%, #3096d6 100%); /* FF3.6+ */\n\
  \    background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,#1f72c0), color-stop(100%,#3096d6)); /* Chrome,Safari4+ */\n\
  \    background: -webkit-linear-gradient(top, #1f72c0 0%,#3096d6 100%); /* Chrome10+,Safari5.1+ */\n\
  \    background: -o-linear-gradient(top, #1f72c0 0%,#3096d6 100%); /* Opera 11.10+ */\n\
  \    background: -ms-linear-gradient(top, #1f72c0 0%,#3096d6 100%); /* IE10+ */\n\
  \    background: linear-gradient(to bottom, #1f72c0 0%,#3096d6 100%); /* W3C */\n\
  \    filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#1f72c0', endColorstr='#3096d6',GradientType=0 ); /* IE6-9 */\n\
  }\n\
  \n\
  .see {\n\
  \    position: fixed;\n\
  \    right: 20px;\n\
  \    bottom: 20px;\n\
  \    border: none;\n\
  \    font-size:14px;\n\
  \    font-weight:bold;\n\
  \    padding-top:10px;\n\
  \    padding-bottom: 10px;\n\
  \    padding-left: 20px;\n\
  \    padding-right: 20px;\n\
  \    background: rgba(255,255,255,0.7);\n\
  \    -webkit-box-shadow: 0px 0px 14px 0px rgba(50, 50, 50, 0.8);\n\
  \    -moz-box-shadow:    0px 0px 14px 0px rgba(50, 50, 50, 0.8);\n\
  \    box-shadow:         0px 0px 14px 0px rgba(50, 50, 50, 0.8);}\n\
  }\n\
  \n\
  \n\
  </style>\n\
  \n\
  </head>\n\
  \n\
  <body>\n\
  \n\
  <div class=\"main\">\n\
  \    <img src=\"data:image/png;base64,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\" />\n\
  \    <h1>Your <a href=\"http://www.skylable.com/products/libres3/\">LibreS3 node</a> is up and running</h1>\n\
  \n\
  \        <p class=\"desc\">Skylable, a complete <a href=\"http://www.skylable.com/how-it-works/\">distributed object-storage</a> software solution.</p><br>\n\
  \n\
  \    <a class=\"learn\" href=\"http://www.skylable.com\">Learn More</a>\n\
  </div>\n\
  \n\
  <a class=\"see\" href=\"https://wiki.skylable.com/wiki/FAQ#LibreS3_Server:_Your_LibreS3_node_is_up_and_running\">Why am I seeing this page?</a>\n\
  \n\
  </body>\n\
  </html>\n\
  "
;;
