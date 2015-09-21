import java.io.*;
import java.util.*;


public class type1Tool {
    public static void main(String[] args) {
        String traceFilePath = args[0];
        HashMap<File, Set<Integer>> traceMap = getTraceMap(traceFilePath);

        String srcPath = args[1];
        HashMap<File, Set<Integer>> lineNumMap = getLineNumMap(srcPath+"/compiler", traceMap);
        lineNumMap.putAll(getLineNumMap(srcPath+"/library", traceMap));
        lineNumMap.putAll(getLineNumMap(srcPath+"/reflect", traceMap));

        String outputFilePath = args[2];
        outputDeadCodeDistribution(outputFilePath, traceMap, lineNumMap);

    }

    private static HashMap<File, Set<Integer>> getTraceMap(String tfp) {
        HashMap<File, Set<Integer>> reMap = new HashMap<File, Set<Integer>>();
        try {
            FileReader fr = new FileReader(new File(tfp));
            BufferedReader bufr = new BufferedReader(fr);
            String line = "";
            while((line = bufr.readLine()) != null && line.contains("scala/src/")) {
                String[] s = line.trim().split(" ");
                File file = new File(s[0]);
                int num = Integer.parseInt(s[1]);
                if(reMap.containsKey(file)) {
                    reMap.get(file).add(num);
                }
                else {
                    Set<Integer> set = new HashSet<Integer>();
                    set.add(num);
                    reMap.put(file, set);
                }
            }
            bufr.close();
            fr.close();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return reMap;
    }

    private static HashMap<File, Set<Integer>> getLineNumMap(String sp, HashMap<File, Set<Integer>> tm) {
        HashMap<File, Set<Integer>> reMap = new HashMap<File, Set<Integer>>();
        File ro = new File(sp);
        File[] farr = ro.listFiles();
        for(int i=0; i<farr.length; i++) {
            if(farr[i].isDirectory())
                reMap.putAll(getLineNumMap(farr[i].getAbsolutePath(), tm));
            else if(farr[i].getName().contains(".scala")) {
                Set<Integer> set = new HashSet<Integer>();
                try {
                    FileReader fr = new FileReader(farr[i]);
                    BufferedReader bufr = new BufferedReader(fr);
                    int it = 1;
                    int state = 1;
                    String line = "";
                    while((line = bufr.readLine()) != null) {
                        if(line.contains("/*") && !line.contains("*/")) {
                            state = 0;
                        }
                        else if(state == 0 && line.contains("*/")) {
                            state = 1;
                        }
                        else if(line.contains("//") || line.trim().length()==0 || (line.contains("/*") && line.contains("*/"))) {
                            //do nothing
                        }
                        else if(state == 1){
                            set.add(it);
                            if(tm.keySet().contains(farr[i]) && (line.contains("import") || line.contains("package") || line.contains("}")))
                                tm.get(farr[i]).add(it);
                        }
                        it++;
                    }
                    reMap.put(farr[i], set);
                    bufr.close();
                    fr.close();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        return reMap;
    }

    private static void outputDeadCodeDistribution(String ofp, HashMap<File, Set<Integer>> tm, HashMap<File, Set<Integer>> lm) {
        try {
            FileWriter fw = new FileWriter(new File(ofp));
            BufferedWriter bufw = new BufferedWriter(fw);
            bufw.write("file path --- number of code lines --- number of dead code lines --- dead code percentage\nline numbers of dead code\n\n");

            for(HashMap.Entry<File, Set<Integer>> entry : lm.entrySet()) {
                if(tm.keySet().contains(entry.getKey())) {
                    int total = entry.getValue().size();
                    bufw.write(entry.getKey().getAbsolutePath()+"\t"+total+"\t");
                    entry.getValue().removeAll(tm.get(entry.getKey()));
                    int rest = entry.getValue().size();
                    bufw.write(rest+"\t"+((double)rest/total)+"\n");
                    List<Integer> list = new ArrayList<Integer>(entry.getValue());
                    Collections.sort(list);
                    for(int i=0; i<list.size(); i++) {
                        bufw.write(list.get(i)+" ");
                    }
                    bufw.write("\n\n");
                }
                else {
                    bufw.write(entry.getKey().getAbsolutePath()+"\t"+entry.getValue().size()+"\t"+entry.getValue().size()+"\t1\n");
                    bufw.write("All\n\n");
                }
            }

            bufw.close();
            fw.close();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }
}
