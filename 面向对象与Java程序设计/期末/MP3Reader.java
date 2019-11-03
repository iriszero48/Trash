package cc.iriszero.mmp;

import android.support.annotation.NonNull;
import org.mozilla.intl.chardet.HtmlCharsetDetector;
import org.mozilla.intl.chardet.nsDetector;
import org.mozilla.intl.chardet.nsICharsetDetectionObserver;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

public class MP3Reader
{
    public ID3v2_TAG id3v2_tag;
    public List<MPEG_FRAME> mf;
    public ID3v1_TAG id3v1_tag;

    public enum ID3_GENRES
    {
        Blues,
        ClassicRock,
        Country,
        Dance,
        Disco,
        Funk,
        Grunge,
        HipHop,
        Jazz,
        Metal,
        NewAge,
        Oldies,
        Other,
        Pop,
        RhythmAndBlues,
        Rap,
        Reggae,
        Rock,
        Techno,
        Industrial,
        Alternative,
        Ska,
        DeathMetal,
        Pranks,
        Soundtrack,
        EuroTechno,
        Ambient,
        TripHop,
        Vocal,
        JazzAndFunk,
        Fusion,
        Trance,
        Classical,
        Instrumental,
        Acid,
        House,
        Game,
        SoundClip,
        Gospel,
        Noise,
        AlternativeRock,
        Bass,
        Soul,
        Punk,
        Space,
        Meditative,
        InstrumentalPop,
        InstrumentalRock,
        Ethnic,
        Gothic,
        Darkwave,
        TechnoIndustrial,
        Electronic,
        PopFolk,
        Eurodance,
        Dream,
        SouthernRock,
        Comedy,
        Cult,
        Gangsta,
        Top40,
        ChristianRap,
        PopFunk,
        Jungle,
        NativeUS,
        Cabaret,
        NewWave,
        Psychedelic,
        Rave,
        ShowTunes,
        Trailer,
        LoFi,
        Tribal,
        AcidPunk,
        AcidJazz,
        Polka,
        Retro,
        Musical,
        RockAndRoll,
        HardRock,
        Folk,
        FolkRock,
        NationalFolk,
        Swing,
        FastFusion,
        Bebop,
        Latin,
        Revival,
        Celtic,
        Bluegrass,
        Avantgarde,
        GothicRock,
        ProgressiveRock,
        PsychedelicRock,
        SymphonicRock,
        Slowrock,
        BigBand,
        Chorus,
        EasyListening,
        Acoustic,
        Humour,
        Speech,
        Chanson,
        Opera,
        ChamberMusic,
        Sonata,
        Symphony,
        BootyBass,
        Primus,
        PornGroove,
        Satire,
        SlowJam,
        Club,
        Tango,
        Samba,
        Folklore,
        Ballad,
        PowerBallad,
        RhythmicSoul,
        Freestyle,
        Duet,
        PunkRock,
        DrumSolo,
        ACappella,
        EuroHouse,
        DanceHall,
        Goa,
        DrumAndBass,
        ClubHouse,
        HardcoreTechno,
        Terror,
        Indie,
        BritPop,
        Negerpunk,
        PolskPunk,
        Beat,
        ChristianGangstaRap,
        HeavyMetal,
        BlackMetal,
        Crossover,
        ContemporaryChristian,
        ChristianRock,
        Merengue,
        Salsa,
        ThrashMetal,
        Anime,
        Jpop,
        Synthpop,
        Abstract,
        ArtRock,
        Baroque,
        Bhangra,
        BigBeat,
        Breakbeat,
        Chillout,
        Downtempo,
        Dub,
        EBM,
        Eclectic,
        Electro,
        Electroclash,
        Emo,
        Experimental,
        Garage,
        Global,
        IDM,
        Illbient,
        IndustroGoth,
        JamBand,
        Krautrock,
        Leftfield,
        Lounge,
        MathRock,
        NewRomantic,
        NuBreakz,
        PostPunk,
        PostRock,
        Psytrance,
        Shoegaze,
        SpaceRock,
        TropRock,
        WorldMusic,
        Neoclassical,
        Audiobook,
        Audiotheatre,
        NeueDeutscheWelle,
        Podcast,
        IndieRock,
        GFunk,
        Dubstep,
        GarageRock,
        Psybient
    }

    public enum FrameID
    {
        AENC,
        ASPI,
        APIC,
        COMM,
        COMR,
        ENCR,
        EQU2,
        ETCO,
        GEOB,
        GRID,
        TIPL,
        LINK,
        MCDI,
        MLLT,
        OWNE,
        PRIV,
        PCNT,
        POPM,
        POSS,
        RBUF,
        RVA2,
        RVRB,
        SEEK,
        SIGN,
        SYLT,
        SYTC,
        TALB,
        TBPM,
        TCOM,
        TCON,
        TCOP,
        TDRC,
        TDEN,
        TDLY,
        TDRL,
        TDTG,
        TENC,
        TEXT,
        TFLT,
        TIT1,
        TIT2,
        TIT3,
        TKEY,
        TLAN,
        TLEN,
        TMCL,
        TMED,
        TMOO,
        TOAL,
        TOFN,
        TOLY,
        TOPE,
        TDOR,
        TOWN,
        TPE1,
        TPE2,
        TPE3,
        TPE4,
        TPOS,
        TPRO,
        TPUB,
        TRCK,
        TRSN,
        TRSO,
        TSIZ,
        TSOA,
        TSOP,
        TSOT,
        TSRC,
        TSSE,
        TSST,
        TXXX,
        UFID,
        USER,
        USLT,
        WCOM,
        WCOP,
        WOAF,
        WOAR,
        WOAS,
        WORS,
        WPAY,
        WPUB,
        WXXX,
        EQUA,
        IPLS,
        RVAD,
        TDAT,
        TIME,
        TORY,
        TRDA,
        TYER
    }

    public enum MPEGAudioVersion
    {
        MPEG_25,
        MPEG_2,
        MPEG_1
    }

    public enum LayerDescription
    {
        LayerIII,
        LayerII,
        LayerI
    }

    public enum ChannelMode
    {
        Stereo,
        JointStereo,
        DualChannel,
        SingleChannel
    }

    public enum Emphasis
    {
        none,
        _5015ms,
        CCITJ17
    }

    public class ID3v2_TAG
    {
        public ID3v2_HEADER hdr;
        public List<ID3v2_FRAME> tf;

        public class ID3v2_HEADER
        {
            public short ver_major;
            public short ver_revision;
            public boolean UNSYNCHRONISATTON_USED;
            public boolean EXTENDED_HEADER_PRESENT;
            public boolean EXPERIMENTAL_TAG;
            public int size;

            public ID3v2_HEADER(byte[] raw, int start)
            {
                start += 3;
                ver_major = UbyteToShort(raw[start++]);
                ver_revision = UbyteToShort(raw[start++]);
                UNSYNCHRONISATTON_USED = UbyteBitToBool(raw[start], 7);
                EXTENDED_HEADER_PRESENT = UbyteBitToBool(raw[start], 6);
                EXPERIMENTAL_TAG = UbyteBitToBool(raw[start++], 5);
                size = ((raw[start] & 0x7f) << 21) | ((raw[start + 1] & 0x7f) << 14) | ((raw[start + 2] & 0x7f) << 7) | (raw[start + 3] & 0x7f);
            }

            @NonNull
            public String toString()
            {
                return "ver_major = " + ver_major + "\n" +
                        "ver_revision = " + ver_revision + "\n" +
                        "UNSYNCHRONISATTON_USED = " + UNSYNCHRONISATTON_USED + "\n" +
                        "EXTENDED_HEADER_PRESENT = " + EXTENDED_HEADER_PRESENT + "\n" +
                        "EXPERIMENTAL_TAG = " + EXPERIMENTAL_TAG + "\n" +
                        "size = " + size + "\n";
            }
        }
        public class ID3v2_FRAME
        {
            public FrameID id;
            public int size;
            public boolean TAG_ALTER_PRESERV;
            public boolean FILE_ALTER_PRESERV;
            public boolean READ_ONLY_FRAME;
            public boolean COMPRESSED_FRAME;
            public boolean ENCRYPTED_FRAME;
            public boolean GROUP_MEMBER_FRAME;
            public byte[] frame_data;

            public ID3v2_FRAME(byte[] raw, int start)
            {
                id = FrameID.valueOf(new String(new char[]{(char)raw[start],(char)raw[start+1],(char)raw[start+2],(char)raw[start+3]}));
                start += 4;
                size = (raw[start] & 0xff) << 24 | (raw[start + 1] & 0xff) << 16 | (raw[start + 2] & 0xff) << 8 | raw[start + 3] & 0xff;
                start += 4;
                TAG_ALTER_PRESERV = UbyteBitToBool(raw[start], 7);
                FILE_ALTER_PRESERV = UbyteBitToBool(raw[start], 6);
                READ_ONLY_FRAME = UbyteBitToBool(raw[start++], 5);
                COMPRESSED_FRAME = UbyteBitToBool(raw[start], 7);
                ENCRYPTED_FRAME = UbyteBitToBool(raw[start], 6);
                GROUP_MEMBER_FRAME = UbyteBitToBool(raw[start++], 5);
                frame_data = Read(raw, start, size);
            }

            @NonNull
            public String toString()
            {
                return "id = " + id + "\n" +
                        "size = " + size + "\n" +
                        "TAG_ALTER_PRESERV = " + TAG_ALTER_PRESERV + "\n" +
                        "FILE_ALTER_PRESERV = " + FILE_ALTER_PRESERV + "\n" +
                        "READ_ONLY_FRAME = " + READ_ONLY_FRAME + "\n" +
                        "COMPRESSED_FRAME = " + COMPRESSED_FRAME + "\n" +
                        "ENCRYPTED_FRAME = " + ENCRYPTED_FRAME + "\n" +
                        "GROUP_MEMBER_FRAME = " + GROUP_MEMBER_FRAME + "\n" +
                        (id != FrameID.APIC ? "frame_data = " + ReadString(frame_data,1,frame_data.length - 1) + "\n" : "");
            }
        }
        public ID3v2_TAG(byte[] raw, int start)
        {
            hdr = new ID3v2_HEADER(raw, start);
            start += 0xa;
            tf = new ArrayList<>();
            for(int i = 0; start < hdr.size && raw[start] != 0; ++i)
            {
                tf.add(new ID3v2_FRAME(raw, start));
                start += 0xa + tf.get(i).size;
            }
        }
        @NonNull
        public String toString()
        {
            StringBuilder out = new StringBuilder("----- ID3v2_HEADER -----\n" + hdr.toString() + "\n");
            for(int i = 0;i<tf.size();++i) out.append("----- ID3v2_FRAME ").append(i).append(" -----\n").append(tf.get(i).toString()).append("\n");
            return out.toString();
        }
    }

    public class ID3v1_TAG
    {
        public String title;
        public String artist;
        public String album;
        public String year;
        public String comment;
        public int track;
        public ID3_GENRES genre;

        public ID3v1_TAG(byte[] raw, int start)
        {
            start += 3;
            title = ReadString(raw, start, 30);
            start += 30;
            artist = ReadString(raw, start, 30);
            start += 30;
            album = ReadString(raw, start, 30);
            start += 30;
            year = ReadString(raw, start, 4);
            start += 4;
            comment = ReadString(raw, start, 28);
            start += 28;
            ++start;
            track = UbyteToShort(raw[start++]);
            for(short gen = UbyteToShort(raw[start]), i = 1; gen<184 && i-- == 1;) genre = ID3_GENRES.values()[gen];
        }

        @NonNull
        public String toString()
        {
            return "----- ID3v1_TAG -----\n" +
                    "title = " + title + "\n" +
                    "artist = " + artist + "\n" +
                    "album = " + album + "\n" +
                    "year = " + year + "\n" +
                    "comment = " + comment + "\n" +
                    "track = " + track + "\n" +
                    "genre = " + genre + "\n\n";
        }
    }

    public class MPEG_FRAME
    {
        public MPEG_HEADER mpeg_hdr;
        public byte[] mpeg_frame_data;

        public class MPEG_HEADER
        {
            public final short[] BitrateTable = new short[]{8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 192, 224, 256, 320, 352, 384, 416, 448};
            public MPEGAudioVersion mpeg_id;
            public LayerDescription layer_id;
            public boolean protection_bit;
            public short bitrate;
            public int frequency;
            public boolean padding_bit;
            public boolean private_bit;
            public ChannelMode channel_mode;
            public boolean IntensityStereo;
            public boolean MSStereo;
            public boolean copyright;
            public boolean original;
            public Emphasis emphasis;
            public int frame_size;
            public MPEG_HEADER(byte[] raw, int start)
            {
                ++start;
                switch((raw[start] & 0x18) >> 3)
                {
                    case 0b00: mpeg_id = MPEGAudioVersion.MPEG_25; break;
                    case 0b10: mpeg_id = MPEGAudioVersion.MPEG_2; break;
                    case 0b11: mpeg_id = MPEGAudioVersion.MPEG_1; break;
                }
                switch((raw[start] & 0x6) >> 1)
                {
                    case 0b01: layer_id = LayerDescription.LayerIII; break;
                    case 0b10: layer_id = LayerDescription.LayerII; break;
                    case 0b11: layer_id = LayerDescription.LayerI; break;
                }

                protection_bit = !UbyteBitToBool(raw[start++],0);
                if(layer_id == LayerDescription.LayerI)
                {
                    if(HByte(raw[start]) == 1) bitrate = BitrateTable[3];
                    else if(HByte(raw[start]) == 2) bitrate = BitrateTable[7];
                    else if(HByte(raw[start]) == 3) bitrate = BitrateTable[9];
                    else if(HByte(raw[start]) == 4) bitrate = BitrateTable[11];
                    else if(HByte(raw[start]) > 4 && HByte(raw[start]) < 15) bitrate = BitrateTable[8 + HByte(raw[start])];
                }
                else if(layer_id == LayerDescription.LayerII)
                {
                    if(HByte(raw[start]) == 1) bitrate = BitrateTable[3];
                    else if(HByte(raw[start]) > 1 && HByte(raw[start]) < 9) bitrate = BitrateTable[3 + HByte(raw[start])];
                    else if(HByte(raw[start]) > 8 && HByte(raw[start]) < 15) bitrate = BitrateTable[4 + HByte(raw[start])];
                }
                else
                {
                    if(mpeg_id == MPEGAudioVersion.MPEG_1)
                    {
                        System.out.println(raw[start]);
                        if(HByte(raw[start]) < 10) bitrate = BitrateTable[2 + HByte(raw[start])];
                        else if(HByte(raw[start]) > 9 && HByte(raw[start]) < 15) bitrate = BitrateTable[3 + HByte(raw[start])];
                    }
                    else if(HByte(raw[start]) < (mpeg_id == MPEGAudioVersion.MPEG_2 ? 15 : 9)) bitrate = BitrateTable[HByte(raw[start])];
                }
                if(mpeg_id == MPEGAudioVersion.MPEG_1)
                {
                    switch((UbyteBitToByte(raw[start],3) << 1) | UbyteBitToByte(raw[start],2))
                    {
                        case 0b00: frequency = 44100; break;
                        case 0b01: frequency = 48000; break;
                        case 0b10: frequency = 32000; break;
                    }
                }
                else if(mpeg_id == MPEGAudioVersion.MPEG_2)
                {
                    switch((UbyteBitToByte(raw[start],3) << 1) | UbyteBitToByte(raw[start],2))
                    {
                        case 0b00: frequency = 22050; break;
                        case 0b01: frequency = 24000; break;
                        case 0b10: frequency = 16000; break;
                    }
                }
                else
                {
                    switch((UbyteBitToByte(raw[start],3) << 1) | UbyteBitToByte(raw[start],2))
                    {
                        case 0b00: frequency = 11025; break;
                        case 0b01: frequency = 12000; break;
                        case 0b10: frequency = 8000; break;
                    }
                }
                padding_bit = UbyteBitToBool(raw[start],1);
                private_bit = UbyteBitToBool(raw[start++],0);
                switch(HByte(raw[start]) >> 2)
                {
                    case 0b00: channel_mode = ChannelMode.Stereo; break;
                    case 0b01: channel_mode = ChannelMode.JointStereo; break;
                    case 0b10: channel_mode = ChannelMode.DualChannel; break;
                    case 0b11: channel_mode = ChannelMode.SingleChannel; break;
                }
                if (channel_mode == ChannelMode.JointStereo)
                {
                    switch(HByte(raw[start]) & 0x3)
                    {
                        case 0b00: IntensityStereo = false; MSStereo = false; break;
                        case 0b01: IntensityStereo = true; MSStereo = false; break;
                        case 0b10: IntensityStereo = false; MSStereo = true; break;
                        case 0b11: IntensityStereo = true; MSStereo = true; break;
                    }
                }
                copyright = UbyteBitToBool(raw[start++],3);
                original = UbyteBitToBool(raw[start++],2);
                switch(raw[start] & 0x3)
                {
                    case 0b00: emphasis = Emphasis.none; break;
                    case 0b01: emphasis = Emphasis._5015ms; break;
                    case 0b11: emphasis = Emphasis.CCITJ17; break;
                }
                frame_size = (((mpeg_id == MPEGAudioVersion.MPEG_1 ? 144 : 72) * bitrate * 1000) / frequency) + (padding_bit ? 1 : 0);
            }

            @NonNull
            public String toString()
            {
                return "mpeg_id = " + mpeg_id + "\n" +
                        "layer_id = " + layer_id + "\n" +
                        "protection_bit = " + protection_bit + "\n" +
                        "bitrate = " + bitrate + "\n" +
                        "frequency = " + frequency + "\n" +
                        "padding_bit = " + padding_bit + "\n" +
                        "private_bit = " + private_bit + "\n" +
                        "channel_mode = " + channel_mode + "\n" +
                        "IntensityStereo = " + IntensityStereo + "\n" +
                        "MSStereo = " + MSStereo + "\n" +
                        "copyright = " + copyright + "\n" +
                        "original = " + original + "\n" +
                        "emphasis = " + emphasis + "\n" +
                        "frame_size = " + frame_size + "\n";
            }
        }

        public MPEG_FRAME(byte[] raw, int start)
        {
            mpeg_hdr = new MPEG_HEADER(raw, start);
            start += 32;
            mpeg_frame_data = Read(raw,start,mpeg_hdr.frame_size);
        }

        @NonNull
        public String toString()
        {
            return mpeg_hdr.toString();
        }
    }

    public MP3Reader(byte[] raw)
    {
        for(int i = 0; i < raw.length; ++i)
        {
            if(raw[i] == 73 && raw[i + 1] == 68 && raw[i+2]==51)
            {
                id3v2_tag = new ID3v2_TAG(raw, i);
                break;
            }
        }
        for(int i = raw.length - 2;i>0;--i)
        {
            if(raw[i] == 84 && raw[i + 1] == 65 && raw[i+2]==71)
            {
                id3v1_tag = new ID3v1_TAG(raw, i);
                break;
            }
        }
        mf = new ArrayList<>();
        for (int start = id3v2_tag == null ? 0 : id3v2_tag.hdr.size + 0xa,i=0; start < raw.length - 128;)
        {
            if (raw[start] == -1 && ((raw[start+1] >> 5) == -1))
            {
                mf.add(new MPEG_FRAME(raw, start));
                start += mf.get(i).mpeg_hdr.frame_size;
                break;
            }
        }
        /*
        for (int start = id3v2_tag.hdr.size + 0xa,i=0; start < raw.length - 128;)
        {
            if (raw[start] == -1 && ((raw[start+1] >> 5) == -1))
            {
                mf.add(new MPEG_FRAME(raw, start));
                start += mf.get(i).mpeg_hdr.frame_size;
                ++i;
            }
        }*/
    }

    @NonNull
    public String toString()
    {
        String out = "";
        out += id3v2_tag == null ? "" : id3v2_tag.toString();
        out += id3v1_tag == null ? "" : id3v1_tag.toString();
        out += "----- MPEG_HEADER 0 -----\n" + mf.get(0).toString();
        return out;
    }

    private short UbyteToShort(byte ubyte)
    {
        return (short)(ubyte & 0xff);
    }

    private byte HByte(byte x)
    {
        return (byte)((x & 0xff) >> 4);
    }

    private boolean UbyteBitToBool(byte ubyte, int bit)
    {
        return ((1 << bit) & ubyte) != 0;
    }

    private byte UbyteBitToByte(byte ubyte, int bit)
    {
        return (byte)((1 << bit) & ubyte);
    }

    private String ReadString(byte[] raw, int start, int count)
    {
        byte[] temp = Read(raw, start, count);
        nsICharsetDetectionObserver cbo = arg0 -> HtmlCharsetDetector.found = true;
        nsDetector det = new nsDetector();
        det.Init(cbo);
        det.DoIt(temp, temp.length,false);
        det.DataEnd();
        String set = det.getProbableCharsets()[0];
        return new String(temp, Charset.forName(set));
    }

    private byte[] Read(byte[] raw, int start, int count)
    {
        byte[] res = new byte[count];
        System.arraycopy(raw, start, res, 0, count);
        return res;
    }
}
