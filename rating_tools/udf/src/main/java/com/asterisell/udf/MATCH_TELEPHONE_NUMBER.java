// SPDX-License-Identifier: GPL-3.0-or-later

package com.asterisell.udf;

import java.io.IOException;

import org.apache.pig.EvalFunc;
import org.apache.pig.PigException;
import org.apache.pig.backend.executionengine.ExecException;
import org.apache.pig.data.DataType;
import org.apache.pig.data.Tuple;
import org.apache.pig.impl.logicalLayer.FrontendException;
import org.apache.pig.impl.logicalLayer.schema.Schema;

import java.util.Map;

/**
 * Given a telephone number, and a map `prefix#value`, return the value of the best matching prefix.
 */
public class MATCH_TELEPHONE_NUMBER extends EvalFunc<String> {
    /**
     * @param input a tuple with exactly two fields.
     * @throws IOException if there are not exactly two fields in a tuple
     */
    @Override
    public String exec(Tuple input) throws IOException {

        if (input.size() != 2) {
            int errCode = 2107;
            String msg = "MATCH_TELEPHONE_NUMBER expected two inputs but received " + input.size() + " inputs.";
            throw new ExecException(msg, errCode, PigException.BUG);
        }
        try {
            String telephoneNumber = (String) input.get(0);
            Map prefixes = (Map) input.get(1);
            return searchBestMatch(telephoneNumber, prefixes);
        } catch (ExecException ee) {
            throw ee;
        }
    }

    private String searchBestMatch(String telephoneNumber, Map prefixes) {
        int l = telephoneNumber.length();

        for (int i = l; i > 0; i--) {
            String m = telephoneNumber.substring(0, i);
            if (prefixes.containsKey(m)) {
                return (String) prefixes.get(m);
            }
        }

        return null;
    }

    @Override
    public boolean allowCompileTimeCalculation() {
        return true;
    }

}
