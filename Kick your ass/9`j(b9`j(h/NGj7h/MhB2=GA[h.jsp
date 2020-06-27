<%@ page language="java" contentType="text/html; charset=UTF-8"
         pageEncoding="UTF-8"%>
<!DOCTYPE HTML>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <title>upload</title>
    <!-- Meta-Tags -->
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta charset="utf-8">
    <script>
        addEventListener("load", function () {
            setTimeout(hideURLbar, 0);
        }, false);

        function hideURLbar() {
            window.scrollTo(0, 1);
        }
    </script>
    <!-- //Meta-Tags -->
    <!-- Stylesheets -->
    <link href="css/font-awesome.css" rel="stylesheet">
    <link href="css/style.css" rel='stylesheet' type='text/css' />
    <style>
        body {
            background-image: url("images/bg.jpg");
            background-repeat: no-repeat;
            background-position: center center;
            background-attachment: fixed;
            background-size: cover;
        }
    </style>
    <!--// Stylesheets -->
<!--fonts-->
<link href="//fonts.googleapis.com/css?family=Source+Sans+Pro:200,200i,300,300i,400,400i,600,600i,700,700i,900,900i&amp;subset=cyrillic,cyrillic-ext,greek,greek-ext,latin-ext,vietnamese" rel="stylesheet">
<!--//fonts-->
</head>

<body>
<%
    if (session.getAttribute("name") != null)
    {
%>

<h1> Upload </h1>
<div class="clear-loading spinner">
    <span></span>
</div>
<div class="w3ls-login box box--big">
    <!-- form starts here -->
    <form method="post" action="/CatCat_war_exploded/UploadServlet" enctype="multipart/form-data">
        <div class="agile-field-txt">
            <label><i class="fa fa-file" aria-hidden="true"></i> File </label>
            <input type="file" name="uploadFile" placeholder="File" required="" />
        </div>

        <input type="submit" name="submit" value="upload">
    </form>
</div>
<%
    }
    else
    {
        response.sendRedirect("index.html");
    }
%>
</body>
</html>
