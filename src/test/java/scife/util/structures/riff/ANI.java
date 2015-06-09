package scife.util.structures.riff;
// to compile: javac1.5 -cp korat.jar ANI.java
//     to run: java1.5 -cp korat.jar:. korat.Korat --class ANI --args 2,1,1,1,1

public class ANI {
    /*public static void main(String[] args) {
      int maxNumIcons = Integer.parseInt(args[0]);
      int maxTitleLength = Integer.parseInt(args[1]);
      int maxAuthorLength = Integer.parseInt(args[2]);
      int maxCSteps = Integer.parseInt(args[3]);
      int maxJifRate = Integer.parseInt(args[4]);
      int maxRateArrayLength = Integer.parseInt(args[5]);
      int maxSeqArrayLength = Integer.parseInt(args[6]);
      ANI a = ANI.generate(maxNumIcons, maxTitleLength, maxAuthorLength, maxCSteps, maxJifRate, maxRateArrayLength, maxSeqArrayLength);
      System.out.println(a);
      }
    */

    public InfoList infoList;
    public ANIHeader aniHeader;
    public Rate rate;
    public Seq seq;
    public FrameList frameList;
    public boolean shouldWeIncludeASeqSubchunk, shouldWeIncludeARateSubchunk;

    // Let's talk about how Korat can't make multiple arrays of the same type
    // and how I have to make three different classes that do nothing but hold
    // integers.

    public static class Integer1 {
	public int i;
    }
    public static class Integer2 {
	public int i;
    }
    public static class Integer3 {
	public int i;
    }

    public boolean repOK() {	
	// numIcons must match everywhere
	if (frameList.icons.length != aniHeader.cFrames)
	    return false;

	for (int i = 0; i < frameList.icons.length; i++) {
	    if (frameList.icons[i] == null)
		return false; // I don't know why I have to check for this, but I'm getting null icons for some reason
	}

	// rate and seq data array lengths must be from 1 to cFrames
	if (shouldWeIncludeARateSubchunk) {
	    if (rate.data.length < 1 || rate.data.length > aniHeader.cSteps)
		return false;
	}
	if (shouldWeIncludeASeqSubchunk) {
	    if (seq.data.length < 1 || seq.data.length > aniHeader.cSteps)
		return false;
	}
	
	
        
	return true;
    }
    
    public static final String id = "RIFF";
    public int size() {
	int size = 4 + infoList.fullSize() + aniHeader.fullSize() + frameList.fullSize();
        if (shouldWeIncludeARateSubchunk)
	    size += rate.fullSize();
	if (shouldWeIncludeASeqSubchunk)
	    size += seq.fullSize();
	return size;
    }
    public static final String type = "ACON";
    public String toString() {
	String s = "ANI(" + id + ", " + size() + ", " + type + ", Data(";
	s += infoList + ", ";
	s += aniHeader + ", ";
	if (shouldWeIncludeARateSubchunk)
	    s += rate + ", ";
	if (shouldWeIncludeASeqSubchunk)
	    s += seq + ", ";
	s += frameList;
	s += "))";
	return s;
    }

    static abstract class ANIData {
	public abstract int fullSize();
    }

    public static class InfoList extends ANIData {
	public int fullSize() {
	    return size() + 8;
	}
	public static final String type = "LIST";
	private int size() {
	    return 4 + title.fullSize() + author.fullSize();
	}
	public String id = "INFO";
	public Title title;
	public Author author;
	public String toString() {
	    return "InfoList(" + type + ", " + size() + ", " + title + ", " + author + ")";
	}
    }

    public static class Title {
	public int fullSize() {
	    return size() + 8;
	}
	public String id = "INAM";
	private int size() {
	    return data.length;
	}
	public int[] data;
	public String toString() {
	    String s = "Title(" + id + ", " + size() + ", ";
	    for (int i = 0; i < data.length; i++)
		s += (char) data[i];
	    s += ")";
	    return s;
	}
    }

    public static class Author {
	public int fullSize() {
	    return size() + 8;
	}
	public String id = "IART";
	private int size() {
	    return data.length;
	}
	public Integer1[] data;
	public String toString() {
	    String s = "Author(" + id + ", " + size() + ", ";
	    for (int i = 0; i < data.length; i++)
		s += (char) data[i].i;
	    s += ")";
	    return s;
	}
    }

    public static class ANIHeader extends ANIData {
	public int fullSize() {
	    return size() + 8;
	}
	public String id = "anih";
	// flags = 1, because 1 is the animation flag for a "Windows format icon/cursor animation"
	public int cbSizeOf = 36, cFrames, cSteps, cx = 0, cy = 0, cBitCount = 0, cPlanes = 0, jifRate, flags = 1;
	private int size() {
	    return 36;
	}
	public String toString() {
	    String s = "ANIHeader(" + id + ", " + size() + ", ";
	    s += "cbSizeOf: " + cbSizeOf + ", ";
	    s += "cFrames: " + cFrames + ", ";
	    s += "cSteps: " + cSteps + ", ";
	    s += "cx: " + cx + ", ";
	    s += "cy: " + cy + ", ";
	    s += "cBitCount: " + cBitCount + ", ";
	    s += "cPlanes: " + cPlanes + ", ";
	    s += "jifRate: " + jifRate + ", ";
	    s += "flags: " + flags;
	    s += ")";
	    return s;
	}
    }

    public static class Rate extends ANIData {
	public int fullSize() {
	    return 8 + size();
	}
	public String id = "rate";
	public Integer2[] data;
	private int size() {
	    return data.length * 4;
	}
	public String toString() {
	    String s = "Rate(" + id + ", " + size() + ", Data(";
	    for (int i = 0; i < data.length; i++) s += data[i].i + ", ";
	    s += "))";
	    return s;
	}
    }

    public static class Seq extends ANIData {
	public int fullSize() {
	    return 8 + size();
	}
	public String id = "seq ";
	public Integer3[] data;
	private int size() {
	    return data.length * 4;
	}
	public String toString() {
	    String s = "Seq(" + id + ", " + size() + ", Data(";
	    for (int i = 0; i < data.length; i++) s += data[i].i + ", ";
	    s += "))";
	    return s;
	}
    }

    public static class FrameList extends ANIData {
	public int fullSize() {
	    return 8 + size();
	}
	public String type = "LIST";
	private int size() {
	    int i = 4;
	    for (int j = 0; j < icons.length; j++) i += icons[j].fullSize();
	    return i;
	}
	public String id = "fram";
	public Icon[] icons;
	public String toString() {
	    String s = "FrameList(" + type + ", " + size() + ", " + id + ", Icons(";
	    for (int i = 0; i < icons.length; i++) s += icons[i] + ", ";
	    s += "))";
	    return s;
	}
    }

    public static class Icon {
	public int fullSize() {
	    return 8 + size();
	}
	public String id = "icon";
	private int size() {
	    return 766;
	}
	public String ICOFile = "[contents of an .ico file]";
	public String toString() {
	    return "Icon(" + id + ", " + size() + ", " + ICOFile + ")";
	}
    }
}
