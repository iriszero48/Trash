package cc.iriszero.mmp;

import android.app.Dialog;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.ColorDrawable;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.design.widget.FloatingActionButton;
import android.support.design.widget.Snackbar;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.text.method.ScrollingMovementMethod;
import android.util.Log;
import android.view.View;
import android.view.Menu;
import android.view.MenuItem;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;
import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class MainActivity extends AppCompatActivity
{
    MP3Reader mp3Reader;
    Dialog dialog;
    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        final EditText path = findViewById(R.id.editText);
        TextView Output = findViewById(R.id.Output);
        Output.setMovementMethod(ScrollingMovementMethod.getInstance());
        Toolbar toolbar = findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);
        FloatingActionButton fab = findViewById(R.id.fab);
        Thread player = new Thread(() ->
        {
            try
            {
                MediaPlayer mediaPlayer = new MediaPlayer();
                mediaPlayer.setDataSource(path.getText().toString().trim());
                mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
                mediaPlayer.prepareAsync();
                mediaPlayer.setOnPreparedListener(mp -> mediaPlayer.start());
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        });
        fab.setOnClickListener(view ->
        {
            final View view1 = view;
            Snackbar.make(view, "Now Loading...", Snackbar.LENGTH_INDEFINITE).setAction("Action", null).show();
            new Thread(() ->
            {
                if(path.getText().toString().contains("http"))
                {
                    try
                    {
                        OkHttpClient okHttpClient = new OkHttpClient();
                        final Request request = new Request.Builder().url(path.getText().toString().trim()).get().build();
                        Call call = okHttpClient.newCall(request);
                        call.enqueue(new Callback()
                        {
                            @Override
                            public void onFailure(@NonNull Call call, @NonNull IOException e)
                            {
                                Log.e("error",e.getMessage());
                            }

                            @Override
                            public void onResponse(@NonNull Call call, @NonNull Response response) throws IOException
                            {
                                mp3Reader = new MP3Reader(response.body() != null ? response.body().bytes() : new byte[0]);
                                Snackbar.make(view1, "Done.", Snackbar.LENGTH_SHORT).setAction("Action", null).show();
                                MainActivity.this.runOnUiThread(() -> Output.setText(mp3Reader.toString()));
                                player.start();
                            }
                        });
                    }
                    catch (Exception e)
                    {
                        Log.e("error:",e.getMessage());
                    }
                }
                else
                {
                    try
                    {
                        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.O)
                        {
                            mp3Reader = new MP3Reader(Files.readAllBytes(Paths.get(path.getText().toString().trim())));
                            Snackbar.make(view1, "Done.", Snackbar.LENGTH_SHORT).setAction("Action", null).show();
                            MainActivity.this.runOnUiThread(() -> Output.setText(mp3Reader.toString()));
                            player.start();
                        }
                    }
                    catch (Exception e)
                    {
                        Log.e("error:",e.getMessage());
                    }
                }
            }).start();
        });
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu)
    {
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {
        int id = item.getItemId();
        if (id == R.id.showAPIC)
        {
            new Thread(this::showImage).start();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }
    public void showImage()
    {
        int i;
        for(i = 0;i<mp3Reader.id3v2_tag.tf.size();++i) if (mp3Reader.id3v2_tag.tf.get(i).id == MP3Reader.FrameID.APIC) break;
        byte[] raw = mp3Reader.id3v2_tag.tf.get(i).frame_data;
        int start;
        for(start = 0;start<raw.length;++start) if((raw[start] == -1 && raw[start + 1]==-40)||(raw[start]==-119 && raw[start+1]==80)) break;
        byte[] data = new byte[raw.length - start];
        System.arraycopy(raw, start, data, 0, data.length);
        Bitmap bitmap = BitmapFactory.decodeByteArray(data,0,data.length);
        MainActivity.this.runOnUiThread(()->
        {
            Dialog builder = new Dialog(MainActivity.this);
            builder.setCanceledOnTouchOutside(true);
            builder.requestWindowFeature(Window.FEATURE_NO_TITLE);
            Objects.requireNonNull(builder.getWindow()).setBackgroundDrawable(new ColorDrawable(android.graphics.Color.TRANSPARENT));
            ImageView imageView = new ImageView(this);
            imageView.setImageBitmap(bitmap);
            builder.addContentView(imageView, new RelativeLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));
            builder.show();
        });
    }
}
