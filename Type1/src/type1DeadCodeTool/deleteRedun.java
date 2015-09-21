/**
 * Created by apple on 15/8/24.
 */

import java.io.*;
import java.util.HashSet;
import java.util.Iterator;

public class deleteRedun extends IOException{
    private static void trimFile(String fp) {
        File infile = new File(fp);
        try {
            BufferedReader in = new BufferedReader(new FileReader(infile));
            HashSet<String> linesSet = new HashSet<String>();
            String temp = null;
            while ((temp = in.readLine()) != null) {
                if (temp.contains("source-") && !linesSet.contains(temp))
                    linesSet.add(temp.split("source-")[1]);
            }
            in.close();

            File outfile = new File(fp);
            try {
                BufferedWriter out = new BufferedWriter(new FileWriter(outfile));
                for(Iterator<String> i=linesSet.iterator(); i.hasNext();) {
                    out.write(i.next()+"\n");
                }
                out.close();
            } catch(IOException e) {
                e.printStackTrace();
            }
            in.close();
        } catch(IOException e) {
            e.printStackTrace();
        }

    }

    public static void main(String[] args) {
        File rd = new File(args[0]);
        File[] file = rd.listFiles();
        for(int i=0; i<file.length; i++) {
            trimFile(file[i].getAbsolutePath());
        }
        HashSet<String> linesSet = new HashSet();
        for(int i=0; i<file.length; i++) {
            try {
                BufferedReader in = new BufferedReader(new FileReader(file[i]));
                String temp = null;
                while((temp = in.readLine()) != null) {
                    if(!linesSet.contains(temp)) {
                        linesSet.add(temp);
                    }
                }
                in.close();
            } catch(IOException e) {
                e.printStackTrace();
            }
        }
        try {
            BufferedWriter out = new BufferedWriter(new FileWriter(new File(args[1])));
            for(Iterator<String> i=linesSet.iterator(); i.hasNext();) {
                out.write(i.next()+"\n");
            }
            out.close();
        } catch(IOException e) {
            e.printStackTrace();
        }
    }
}
