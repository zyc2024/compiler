import java.io.FileWriter;
import java.io.IOException;

class Writer {
    public static void main(String[] args) throws IOException{
        FileWriter fw = new FileWriter("long_long.evo");
        fw.write("main(){x=[");
        int MILLION = 1000000;
        int SIZE_ROUGHLY_1GB= 1000000000 / 2;
        for (int i = 0; i < SIZE_ROUGHLY_1GB; i++){
            fw.write("0,");
        }
        fw.write("];}");
        fw.close();
    }
}