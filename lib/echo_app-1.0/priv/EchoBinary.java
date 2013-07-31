import java.io.*;

import com.mysql.jdbc.Messages;

public final class EchoBinary {

    /*
     * test code erlang {ok, Fd} = file:open("in", [raw, write]). L =
     * "select 1". file:write(Fd, <<(byte_size(list_to_binary(L))):24/little,
     * (list_to_binary(L))/binary>>).
     */
    /**
     * @param null This program is for Erlang open_port call example
     * @throws IOException
     */
    private final OutputStream log;
    private final InputStream is;
    private final PrintStream os;
    private byte[] header;

    public EchoBinary(String logfile, InputStream is, PrintStream os)
            throws FileNotFoundException {
        this.log = new FileOutputStream(logfile, true);
        // this.is = new FileOutputStream("in");
        this.is = is;
        this.os = os;
        this.header = new byte[3];
    }

    public static void main(String[] args) throws IOException {

        EchoBinary eb = new EchoBinary("log", System.in, System.out);
        eb.loop();
    }

    public void loop() throws IOException {
        byte[] body = null;
        while (true) {
            /*
             * header : 3 bytes body :
             */
            readFully(this.is, this.header, 0, this.header.length);

            int packetLength = (this.header[0] & 0xff)
                    + ((this.header[1] & 0xff) << 8)
                    + ((this.header[2] & 0xff) << 16);

            this.log.write(("Header is ok, length is " + packetLength + "\n")
                    .getBytes());

            body = new byte[packetLength + 1];
            readFully(this.is, body, 0, body.length - 1);
            CharSequence s = new String(body);
            this.log.write(("Body is [" + s + "]\n").getBytes());
            this.os.println(String.valueOf(packetLength));
            this.os.println("OK");
            this.os.flush();
            body = null;
        }
    }

    public int readFully(InputStream in, byte[] b, int off, int len)
            throws IOException {
        if (len < 0) {
            throw new IndexOutOfBoundsException();
        }

        int n = 0;

        while (n < len) {
            int count = in.read(b, off + n, len - n);
            this.log.write(("read " + count + " " + n + " " + len + "\n")
                    .getBytes());
            if (count < 0) {
                throw new EOFException();
            }

            n += count;
        }

        return n;
    }
}
