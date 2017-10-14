package com.company;

import java.util.*;
import java.util.stream.Collectors;

public class Solution {

    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
//	    String input = scan.nextLine();
	    String input = "4523685632475678236257843563478";
	    List<Character> invalidInput = new ArrayList<>();
	    Map<Character, Integer> occurances = new HashMap<>();
	    for(Character c: input.toCharArray()) {
	        if(!Character.isDigit(c) ) {
	            if(!invalidInput.contains(c)) invalidInput.add(c);
            } else {
                if(!occurances.containsKey(c)) {
                    occurances.put(c, 0);
                }

                occurances.put(c, occurances.get(c) + 1);
            }
        }

        if(!invalidInput.isEmpty()) {
	        System.out.println("Invalid input: " + getStringRepresentation(invalidInput));
        } else {

            Set<Character> used = new HashSet<>();
            List<Integer> results = new ArrayList<>();
            for(Character c: input.toCharArray()) {
                if(!used.contains(c)) {
                    results.add(pow(Integer.parseInt(c.toString()), occurances.get(c)));
                    used.add(c);
                } else {
//                results.add(Integer.parseInt(c.toString()));
                }
            }

            List<Character> resChars = results.stream().map(in -> {
                return fromInt(in);
            }).collect(Collectors.toList());

            String beforeEncode = getStringRepresentation(resChars);
            Integer cipherKey = results.stream().mapToInt(Integer::intValue).sum();
	        System.out.println(VigenereCipher.encrypt(beforeEncode, cipherKey.toString()));
        }

    }

    static String getStringRepresentation(List<Character> list)
    {
        StringBuilder builder = new StringBuilder(list.size());
        for(Character ch: list)
        {
            builder.append(ch);
        }
        return builder.toString();
    }

    static int pow(int a, int b) {
        int result = 1;
        for (int i = 1; i <= b; i++) {
            result *= a;
        }
        return result;
    }

    static String cipher(String msg, int shift){
        shift = shift % 26;
        String s = "";
        int len = msg.length();
        for(int x = 0; x < len; x++){
            char c = (char)(msg.charAt(x) + shift);
            if (c > 'Z')
                s += (char)(msg.charAt(x) - (26-shift));
            else
                s += (char)(msg.charAt(x) + shift);
        }
        return s;
    }

    static char fromInt(int i) {
        int res = i % 26;
        if(res == 0) return 'Z';
        else return (char)(res + (int)'A' - 1);
    }

    public static class VigenereCipher {
//        public static void main(String[] args) {
//            String key = "VIGENERECIPHER";
//            String ori = "Beware the Jabberwock, my son! The jaws that bite, the claws that catch!";
//            String enc = encrypt(ori, key);
//            System.out.println(enc);
//            System.out.println((enc, key));
//        }

        static String encrypt(String text, final String key) {
            String res = "";
            text = text.toUpperCase();
            for (int i = 0, j = 0; i < text.length(); i++) {
                char c = text.charAt(i);
                if (c < 'A' || c > 'Z') continue;
//                res += (char)((c + Integer.parseInt(((Character)key.charAt(j)).toString()) % 26 + 'A'));
                res += fromInt(c + Integer.parseInt(((Character)key.charAt(j)).toString()) - 'A' + 1);
                j = ++j % key.length();
            }
            return res;
        }

//        static String decrypt(String text, final String key) {
//            String res = "";
//            text = text.toUpperCase();
//            for (int i = 0, j = 0; i < text.length(); i++) {
//                char c = text.charAt(i);
//                if (c < 'A' || c > 'Z') continue;
//                res += (char)((c - key.charAt(j) + 26) % 26 + 'A');
//                j = ++j % key.length();
//            }
//            return res;
//        }

    }


}
