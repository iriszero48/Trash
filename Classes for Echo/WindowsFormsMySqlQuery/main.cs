using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using MySql.Data.MySqlClient;
using System.Threading;

namespace WindowsFormsApp2
{
    public partial class Form1 : Form
    {
        private static readonly object Lock = new object();

        public Form1()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, EventArgs e)
        {
            if (string.IsNullOrEmpty(InputBox.Text)) return;

            Button.Enabled = false;
            Button.Text = @"查询中";

            new Thread((() =>
            {
                lock (Lock)
                {
                    var conn = new MySqlConnection(
                        "data source=localhost;database=filemd5database;user id=root;password=toor;pooling=false;charset=utf8");
                    var cmd = new MySqlCommand("select * from fmd_200816 where path like @p limit 500", conn);
                    cmd.Parameters.Add(new MySqlParameter("@p", $"%{InputBox.Text}%"));

                    try
                    {
                        Output.Invoke(new Action((() => Output.Rows.Clear())));
                        conn.Open();
                        var reader = cmd.ExecuteReader();
                        while (reader.Read())
                        {
                            var dt = Enumerable.Range(0, 5).Select(x => reader.GetString(x)).ToArray();
                            Output.Invoke(new Action(() =>
                                Output.Rows.Add(dt[0], dt[1], dt[2], Convert.ToInt64(dt[3]), dt[4])));
                        }
                    }
                    catch (Exception exception)
                    {
                        MessageBox.Show(exception.Message, @"错误", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                    finally
                    {
                        conn.Close();
                        Button.Invoke(new Action((() =>
                        {
                            Button.Enabled = true;
                            Button.Text = @"查询";
                        })));
                    }
                }
            })).Start();
        }
    }
}
