package it.unipi.hadoop;

import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.NullWritable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.GenericOptionsParser;

public class Stripes {
    public static class StripesMapper extends Mapper<LongWritable, Text, Text, Text> {
        private final Text outputKey = new Text();
        private final Text outputValue = new Text();
        private int windowSize;

        // setup function is used to retrieve window size value
        public void setup(Context context) throws IOException, InterruptedException {
            windowSize = context.getConfiguration().getInt("Stripes.window.size", 5); // default value : 5
        }
        // inputs are 782 text files, name lineXXX where XXX is a number from 000 to 781
        // each input file contains a text on multiple lines, including numbers,
        // punctuation ...
        // split every line, i.e. a record for map() function, in every text file into
        // tokens, i.e. space-delimited sequences of characters

        // takes a record (a line) from one of the input file and processes it
        // dividing each line into tokens, for example: "hello world" -> ["hello",
        // "world"]
        // neighboorhood chosen: windows of terms of a given window size W near the
        // current term (W before and after)
        // map function returns for example: starting from "hi how are" with W=2 -> (hi,
        // {how=1,are=1}) (how, {hi=1, are=1}) (are, {hi=1, how=1})

        @Override
        public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
            // value is a line from the input file
            String line = value.toString().trim(); // "hi how are you"
            if (line == null || line.length() == 0 || line.isEmpty()){
                return;
            }

            String[] tokens = line.split("\\W+"); // {"hi", "how", "are", "you"}
            for (int i = 0; i < tokens.length; i++) { // for each term that belongs to the line
                String w = tokens[i]; // take the single word

                Map<String, Integer> map = new HashMap<String, Integer>(); // create a map for the current term
                                                                           // neighborhood
                for (int j = 1; j <= windowSize; j++) {
                    Integer k = 0;
                    if (i + j < tokens.length) {
                        k = map.containsKey(tokens[i + j]) ? map.get(tokens[i + j]) : 0;
                        if(!tokens[i+j].isEmpty()) 
                            map.put(tokens[i + j], k + 1); // saving frequency of the term in the neighborhood
                    }
                    if (i - j >= 0) {
                        k = map.containsKey(tokens[i - j]) ? map.get(tokens[i - j]) : 0;
                        if(!tokens[i-j].isEmpty()) 
                            map.put(tokens[i - j], k + 1);
                    }
                }
                outputKey.set(w); // setting current term as key
                outputValue.set(map.toString()); // emit the term and its neighborhood
                // example:
                // key: "hi", value: "{how=1, are=1}"
                context.write(outputKey, outputValue);
            }
        }
    }

    public static class StripesReducer extends Reducer<Text, Text, Text, Text> {
        @Override
        public void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
            Map<String, Integer> map = new HashMap<String, Integer>(); // create a map for the current term neighborhood
            Integer j = 0;

            for (Text value : values) {
                // value is a single neighborhood, a single associative array
                String val = value.toString();
                val = val.substring(1); // delete initial bracket
                val = val.substring(0, val.length() - 1); // delete final bracket
                // if there were some other spaces
                if (val.startsWith("  ")) {
                    val = val.substring(2);
                }
                if (val.startsWith(" ")) {
                    val = val.substring(1);
                }

                String[] tokens = val.toString().split(","); // "how=3, you=2, are=8"
                for (String token : tokens) {
                    String[] pair = token.split("="); // "how" "3" "you" "2" "are" "8"
                    if (pair.length == 2) {
                        Integer k = 0;
                        if (pair[0].startsWith(" ")) {
                            pair[0] = pair[0].substring(1);
                        }
                        k = map.containsKey(pair[0]) ? map.get(pair[0]) : 0;
                        map.put(pair[0], k + Integer.parseInt(pair[1])); // saving frequency of the term in the
                                                                         // neighborhood
                    }
                }

            }
            context.write(key, new Text(map.toString())); // key: hi value: {how=122, you=22, are=81, context=1, mail =
                                                          // 32}
        }
    }

    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        String[] otherArgs = new GenericOptionsParser(conf, args).getRemainingArgs();
        if (otherArgs.length != 4) {
            System.err.println("Usage: Stripes <window_size> <input> <output> <num_reducers>");
            System.exit(1);
        }
        System.out.println("args[0]: <window_size>=" + otherArgs[0]);
        System.out.println("args[1]: <input>=" + otherArgs[1]);
        System.out.println("args[2]: <output>=" + otherArgs[2]);
        System.out.println("args[3]: <num_reducers>=" + otherArgs[3]);

        Job job = Job.getInstance(conf, "Stripes");
        job.setJarByClass(Stripes.class);

        // set mapper/reducer
        job.setMapperClass(StripesMapper.class);
        job.setReducerClass(StripesReducer.class);

        // define mapper's output key-value
        job.setMapOutputKeyClass(Text.class);
        job.setMapOutputValueClass(Text.class);

        // define reducer's output key-value
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        // set number of reducers
        job.setNumReduceTasks(Integer.parseInt(otherArgs[3]));

        // set window size for moving average calculation
        int windowSize = Integer.parseInt(otherArgs[0]);
        job.getConfiguration().setInt("Stripes.window.size", windowSize);

        // define I/O
        FileInputFormat.addInputPath(job, new Path(otherArgs[1]));
        FileOutputFormat.setOutputPath(job, new Path(otherArgs[2]));

        job.setInputFormatClass(TextInputFormat.class);
        job.setOutputFormatClass(TextOutputFormat.class);

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
