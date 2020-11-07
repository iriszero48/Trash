<%@ page import="java.sql.*" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <title>Login</title>
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
<h1>
<%
    String name = request.getParameter("name");
    String password = request.getParameter("password");
    String submit = request.getParameter("submit");


    Class.forName("oracle.jdbc.driver.OracleDriver");
    Connection conn=DriverManager.getConnection("jdbc:oracle:thin:@192.168.244.137:1521:orcl","root","toor");


        if ("login".equals(submit))
        {
            Statement stmt=conn.createStatement();
            ResultSet rs = stmt.executeQuery("select * from USERS");
            while (rs.next())
            {
                if (name.equals(rs.getString("username")) && password.equals(rs.getString("password")))
                {
                    session.setAttribute("name", name);
                    response.sendRedirect("cat.jsp");
                }
            }
%>
Invalid username or password.
<script type="application/javascript">
    setTimeout(() => window.location.href = 'index.html', 3000);
</script>
<%
        }
        else if ("register".equals(submit))
        {
            String confirm = request.getParameter("confirm");
            if(!password.equals(confirm))
            {
%>
Password doesn't match confirmation
<script type="application/javascript">
    setTimeout(() => window.location.href = 'register.html', 3000);
</script>
<%
            }
            else
            {
            PreparedStatement stmt = conn.prepareStatement("INSERT INTO USERS VALUES (?,?)");
            stmt.setString(1, name);
            stmt.setString(2, password);

                try
                {
                    if (stmt.executeUpdate() == 0)
                    {

                    }
                    else
                    {
%>
User Registered Successfully!
        <script type="application/javascript">
            setTimeout(() => window.location.href = 'index.html', 3000);
        </script>
<%
                    }
            }
            catch (SQLException e)
            {
%>
Username already exists
<script type="application/javascript">
    setTimeout(() => window.location.href = 'register.html', 3000);
</script>
<%
            }
        }
        }
        else
            {
                response.sendRedirect("index.html");
            }

%>
</h1>
</body>
</html>
