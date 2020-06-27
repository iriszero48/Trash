<%@ page import="java.io.File" %><%--
  Created by IntelliJ IDEA.
  User: tang9
  Date: 2020/6/25 0025
  Time: 11:07
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Cat</title>
    <style type="text/css">
        img {
            max-width:260px;
            max-height:260px;
        }
        li {
            width:260px;
            height:260px;
            float:left;
            margin-left:10px;
            margin-top:10px;
            list-style-type:none;
            text-align:center;
        }
    </style>
</head>
<body>
<%
if (session.getAttribute("name") != null)
{
%>
<div style="display: grid; grid-template-rows: 64px auto">
    <div style="background-color: rgb(71,74,79);color: white;display: flex;flex-direction: row;justify-content: flex-end;text-align: center;font-size: 2rem;" id="top">
        <a style="color: white;margin: auto 20px;text-decoration: none;" href="upload.jsp">upload</a>
        <a style="color: white;margin: auto 20px;text-decoration: none;" href="index.html">logout</a>
        <div style="margin: auto 20px"><%=session.getAttribute("name")%></div>
    </div>
    <div id="cat">
        <ul>
            <%
                File dir = new File("Z:\\CatCat\\out\\artifacts\\CatCat_war_exploded\\upload");
                for (String f : dir.list())
                {
            %>
            <li><img src="upload/<%=f%>" /></li>
            <%
                }
            %>
        </ul>
    </div>
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
