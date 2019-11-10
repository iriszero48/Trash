using UnityEngine;
using System;
public class NewBehaviourScript : MonoBehaviour
{
    private string filename;
    private string guess;
    private bool first;
    private bool senc;
    private bool three;
    private bool eight;
    private bool vot;
    private string input;
    private int times;
    private string result;
    private Texture2D txt;
    private bool IsRepeat()
    {
        for (int i = 0; i < 4; ++i)
        {
            for (int j = 0; j < 4; ++j)
            {
                if (guess[i] == guess[j] && i != j)
                {
                    return true;
                }
            }
        }
        return false;
    }
    private void Jugde()
    {
        int m = 0, n = 0;
        for (int i = 0; i < 4; ++i)
        {
            if (input[i] == guess[i])
            {
                ++m;
            }
            for (int j = 0; j < 4; ++j)
            {
                if (input[i] == guess[j] && j != i)
                {
                    ++n;
                }
            }
        }
        result = m + "A" + n + "B";
        if (m == 4)
        {
            vot = true;
            senc = false;
        }
        ++times;
    }
    void Start ()
    {
        do
        {
            guess = Convert.ToString(new System.Random().Next(1000, 10000));
        } while (IsRepeat() || guess[0] == '0');
        filename = "start.png";
        string path = Application.dataPath +"//"+ filename;
        WWW www = new WWW("file://" + path);
        txt = new Texture2D(640, 480);
        txt = www.texture;
        first = true;
        senc = false;
        three = false;
        times = 0;
        vot = false;
        eight = true;
        input = "";
        result = "";
        Screen.SetResolution(640, 480, false);
    }
	void Update ()
    {
		
	}
    private void OnGUI()
    {

        GUI.DrawTexture(new Rect(0, 0, 640, 480), txt);
        if (first)
        {
            if (GUI.Button(new Rect(0, 0, 640, 480), "1、随机抽取一个 4 位整数 k，要求这个四位数没有重复数字，最高位不是 0\n 2、提示玩家，猜一个数字\n3、根据玩家猜的数，与 k 比较，判断玩家猜的数字中位置正确的数字个数 m 和数字正\n确而位置不对的数的个数 n，并以 mAnB 的形式输出\n")) 
            {
                first = false;
                senc = true;
                filename = "ui.png";
                string path = Application.dataPath + "//" + filename;
                WWW www = new WWW("file://" + path);
                txt = new Texture2D(640, 480);
                txt = www.texture;
            }
        }
        if(senc)
        {
            input = GUI.TextField(new Rect(Screen.width / 2+12, Screen.height / 2-30-4, 198, 28), input);
            if(GUI.Button(new Rect(Screen.width / 2 + 12, Screen.height / 2 - 30 - 4+28+3, 198, 28), result))
            {
                Jugde();
            }
        }
        if(vot)
        {
            if (GUI.Button(new Rect(0, 0, 640, 480), "你太棒了，这个数 字就是" + guess + "，你一共猜了" + (times + 1) + "次哦")) 
            {

            }
        }
        if(three)
        {
            if (GUI.Button(new Rect(0, 0, 640, 480), "你已经猜了 15 次啦，是不是方法不对啊？ 休息一会再来玩吧！"))
            {
                
            }
        }
        if (times == 7 && eight == true)
        {
            if (GUI.Button(new Rect(0, 0, 640, 480), "你已经猜了 8 次了，还要继续吗？"))
            {
                eight = false;
            }
        }
        if(times==15)
        {
            senc = false;
            three = true;
        }
    }
}
