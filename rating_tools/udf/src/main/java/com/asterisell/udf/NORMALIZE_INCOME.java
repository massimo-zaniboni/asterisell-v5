// SPDX-License-Identifier: GPL-3.0-or-later

package com.asterisell.udf;

import org.apache.pig.EvalFunc;
import org.apache.pig.PigException;
import org.apache.pig.backend.executionengine.ExecException;
import org.apache.pig.data.Tuple;

import java.io.IOException;

/**
 * Given an income as a string, normalize to a default format like:
 * - from XY to XY.0
 * - from X.YZ000 to X.YZ
 */
public class NORMALIZE_INCOME extends EvalFunc<String> {

    public String exec(Tuple input) throws IOException {
        if (input.size() != 1) {
            int errCode = 2107;
            String msg = "NORMALIZE_INCOME expected one input but received " + input.size() + " inputs.";
            throw new ExecException(msg, errCode, PigException.BUG);
        }

        try {
            String s = (String) input.get(0);

            int p1 = s.indexOf('.');
            if (p1 == -1) {
                return s + ".0";
            }

            int p2 = s.length() - 1;
            while(s.charAt(p2) == '0') {
                p2--;
            }

            String r = s.substring(0, p2 + 1);

            if (r.endsWith(".")) {
                r = r + "0";
            }

            return r;
        } catch (Exception e) {
            throw new IOException("Caught exception processing input row ", e);
        }
    }
}
