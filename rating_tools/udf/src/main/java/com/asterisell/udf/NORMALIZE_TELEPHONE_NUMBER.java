// SPDX-License-Identifier: GPL-3.0-or-later

package com.asterisell.udf;

import java.io.IOException;

import org.apache.pig.EvalFunc;
import org.apache.pig.data.Tuple;
import org.apache.pig.PigException;
import org.apache.pig.backend.executionengine.ExecException;

/**
 * Given a default national telephone prefix like 27:
 * - 0X became 27X (national call)
 * - 00X became X (international call)
 * - +X became X (international call)
 */
public class NORMALIZE_TELEPHONE_NUMBER extends EvalFunc<String> {

    public String exec(Tuple input) throws IOException {
        if (input.size() != 2) {
            int errCode = 2107;
            String msg = "NORMALIZE_TELEPHONE_NUMBER expected two input but received " + input.size() + " inputs.";
            throw new ExecException(msg, errCode, PigException.BUG);
        }

        try {
            String defaultCode = (String) input.get(0);
            String tn = (String) input.get(1);

            if (tn.startsWith("00")) {
                return tn.substring(2);
            }
            if (tn.startsWith("+")) {
                return tn.substring(1);
            }
            if (tn.startsWith("0")) {
                return defaultCode + tn.substring(1);
            }
            return tn;
        } catch (Exception e) {
            throw new IOException("Caught exception processing input row ", e);
        }
    }
}
