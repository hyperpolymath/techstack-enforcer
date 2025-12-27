-------------------------------------------------------------------------------
-- Techstack Enforcer - Learning Mode Analyzer Implementation
-------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Techstack_Notify;

package body Techstack_Learning is

   --  Known memory-safe languages
   Memory_Safe_Langs : constant String :=
     "Rust Ada SPARK Haskell OCaml Elm ReScript Elixir Erlang ";

   --  Languages with strong static typing
   Static_Typed_Langs : constant String :=
     "Rust Ada SPARK Haskell OCaml Elm ReScript TypeScript Go Java Kotlin ";

   procedure Initialize is
   begin
      Learn_DB.Count := 0;
      Learn_DB.Enabled := True;
      Learn_DB.Entries := (others => Null_Learn_Entry);
   end Initialize;

   function Extension_To_Language (Ext : String) return String is
      E : constant String := Trim (Ext, Ada.Strings.Both);
   begin
      if E = "py" or E = "pyw" then return "Python";
      elsif E = "rb" then return "Ruby";
      elsif E = "php" then return "PHP";
      elsif E = "c" then return "C";
      elsif E = "cpp" or E = "cc" or E = "cxx" then return "C++";
      elsif E = "h" or E = "hpp" then return "C/C++";
      elsif E = "js" or E = "mjs" or E = "cjs" then return "JavaScript";
      elsif E = "ts" then return "TypeScript";
      elsif E = "jsx" then return "JavaScript-JSX";
      elsif E = "tsx" then return "TypeScript-TSX";
      elsif E = "rs" then return "Rust";
      elsif E = "adb" or E = "ads" then return "Ada";
      elsif E = "hs" or E = "lhs" then return "Haskell";
      elsif E = "ex" or E = "exs" then return "Elixir";
      elsif E = "erl" or E = "hrl" then return "Erlang";
      elsif E = "ml" or E = "mli" then return "OCaml";
      elsif E = "res" or E = "resi" then return "ReScript";
      elsif E = "elm" then return "Elm";
      elsif E = "go" then return "Go";
      elsif E = "java" then return "Java";
      elsif E = "kt" or E = "kts" then return "Kotlin";
      elsif E = "pl" or E = "pm" then return "Perl";
      elsif E = "sh" or E = "bash" then return "Shell";
      else return "Unknown";
      end if;
   end Extension_To_Language;

   function Parse_Vulnerability (Vuln_Str : String) return Vuln_Category is
      V : constant String := Trim (Vuln_Str, Ada.Strings.Both);
   begin
      if Index (V, "buffer") > 0 or Index (V, "overflow") > 0 or
         Index (V, "out-of-bounds") > 0 or Index (V, "heap") > 0
      then
         return Buffer_Overflow;
      elsif Index (V, "null") > 0 or Index (V, "nullptr") > 0 or
            Index (V, "nil") > 0 or Index (V, "none") > 0
      then
         return Null_Dereference;
      elsif Index (V, "race") > 0 or Index (V, "toctou") > 0 or
            Index (V, "concurrent") > 0 or Index (V, "thread") > 0
      then
         return Race_Condition;
      elsif Index (V, "injection") > 0 or Index (V, "xss") > 0 or
            Index (V, "sql") > 0 or Index (V, "command") > 0
      then
         return Injection_Attack;
      elsif Index (V, "use-after-free") > 0 or Index (V, "double-free") > 0 or
            Index (V, "uaf") > 0 or Index (V, "dangling") > 0
      then
         return Use_After_Free;
      elsif Index (V, "integer") > 0 and
            (Index (V, "overflow") > 0 or Index (V, "underflow") > 0)
      then
         return Integer_Overflow;
      elsif Index (V, "type") > 0 and Index (V, "confusion") > 0 then
         return Type_Confusion;
      elsif Index (V, "leak") > 0 or Index (V, "memory") > 0 then
         return Memory_Leak;
      elsif Index (V, "input") > 0 or Index (V, "validate") > 0 or
            Index (V, "sanitize") > 0
      then
         return Unvalidated_Input;
      else
         return Other_Vuln;
      end if;
   end Parse_Vulnerability;

   function Is_Memory_Safe (Lang : String) return Boolean is
      L : constant String := Trim (Lang, Ada.Strings.Both) & " ";
   begin
      return Index (Memory_Safe_Langs, L) > 0;
   end Is_Memory_Safe;

   function Has_Static_Typing (Lang : String) return Boolean is
      L : constant String := Trim (Lang, Ada.Strings.Both) & " ";
   begin
      return Index (Static_Typed_Langs, L) > 0;
   end Has_Static_Typing;

   procedure Record_Finding
     (Vuln        : Vuln_Category;
      Source_File : String;
      Details     : String)
   is
      pragma Unreferenced (Details);
      Lang      : constant String := "Unknown";  --  Would extract from Source_File
      Found     : Boolean := False;
      Found_Idx : Natural := 0;
   begin
      if not Learn_DB.Enabled then
         return;
      end if;

      --  Check if we already have this pattern
      for I in 1 .. Learn_DB.Count loop
         if Learn_DB.Entries (I).Vuln_Type = Vuln then
            --  Update existing entry
            Learn_DB.Entries (I).Occurrences :=
              Learn_DB.Entries (I).Occurrences + 1;
            Found := True;
            Found_Idx := I;
            exit;
         end if;
      end loop;

      if not Found and then Learn_DB.Count < Max_Learn_Entries then
         --  Add new entry
         Learn_DB.Count := Learn_DB.Count + 1;
         declare
            E : Learn_Entry renames Learn_DB.Entries (Learn_DB.Count);
            L_Len : constant Natural := Natural'Min (Lang'Length, 32);
            F_Len : constant Natural := Natural'Min (Source_File'Length, Max_Pattern_Length);
         begin
            E.Vuln_Type := Vuln;
            E.Source_Lang := (others => ' ');
            E.Source_Lang (1 .. L_Len) := Lang (Lang'First .. Lang'First + L_Len - 1);
            E.Lang_Len := L_Len;
            E.File_Pattern := (others => ' ');
            E.File_Pattern (1 .. F_Len) := Source_File (Source_File'First .. Source_File'First + F_Len - 1);
            E.Pattern_Len := F_Len;
            E.Occurrences := 1;
            E.Last_Seen := 0;  --  Would set to current timestamp
         end;
      end if;

      pragma Unreferenced (Found_Idx);
   end Record_Finding;

   function Get_Recommendation (Vuln : Vuln_Category) return Recommendation is
      Result : Recommendation;
   begin
      Result.Languages := (others => ' ');
      Result.Explanation := (others => ' ');

      case Vuln is
         when Buffer_Overflow =>
            Result.Languages (1 .. 28) := "Rust, Ada/SPARK, Haskell   ";
            Result.Lang_Len := 28;
            Result.Confidence := 95;
            Result.Explanation (1 .. 50) :=
              "Memory-safe languages prevent buffer overflows    ";
            Result.Expl_Len := 50;

         when Null_Dereference =>
            Result.Languages (1 .. 32) := "Rust, Haskell, ReScript, Elm   ";
            Result.Lang_Len := 32;
            Result.Confidence := 92;
            Result.Explanation (1 .. 48) :=
              "Option types eliminate null pointer exceptions  ";
            Result.Expl_Len := 48;

         when Race_Condition =>
            Result.Languages (1 .. 28) := "Elixir, Erlang, Rust, Ada  ";
            Result.Lang_Len := 28;
            Result.Confidence := 88;
            Result.Explanation (1 .. 52) :=
              "Actor model and ownership prevent data races      ";
            Result.Expl_Len := 52;

         when Injection_Attack =>
            Result.Languages (1 .. 28) := "Haskell, Rust, ReScript    ";
            Result.Lang_Len := 28;
            Result.Confidence := 85;
            Result.Explanation (1 .. 54) :=
              "Strong typing and parsing libraries prevent inject";
            Result.Expl_Len := 54;

         when Use_After_Free =>
            Result.Languages (1 .. 20) := "Rust, Ada/SPARK     ";
            Result.Lang_Len := 20;
            Result.Confidence := 98;
            Result.Explanation (1 .. 44) :=
              "Ownership/borrowing prevents use-after-free  ";
            Result.Expl_Len := 44;

         when Integer_Overflow =>
            Result.Languages (1 .. 20) := "Ada/SPARK, Rust     ";
            Result.Lang_Len := 20;
            Result.Confidence := 90;
            Result.Explanation (1 .. 48) :=
              "Range types and overflow checks catch errors    ";
            Result.Expl_Len := 48;

         when Type_Confusion =>
            Result.Languages (1 .. 32) := "Haskell, OCaml, Rust, ReScript ";
            Result.Lang_Len := 32;
            Result.Confidence := 93;
            Result.Explanation (1 .. 48) :=
              "Strong type systems prevent type confusion      ";
            Result.Expl_Len := 48;

         when Memory_Leak =>
            Result.Languages (1 .. 36) := "Rust, Ada/SPARK, Haskell, Elixir  ";
            Result.Lang_Len := 36;
            Result.Confidence := 87;
            Result.Explanation (1 .. 52) :=
              "RAII/ownership or GC with immutability prevent leak";
            Result.Expl_Len := 52;

         when Unvalidated_Input =>
            Result.Languages (1 .. 24) := "Haskell, Rust, ReScript ";
            Result.Lang_Len := 24;
            Result.Confidence := 82;
            Result.Explanation (1 .. 48) :=
              "Parser combinators and strong types validate    ";
            Result.Expl_Len := 48;

         when Other_Vuln =>
            Result.Languages (1 .. 16) := "Rust, Ada/SPARK ";
            Result.Lang_Len := 16;
            Result.Confidence := 70;
            Result.Explanation (1 .. 40) :=
              "General memory-safe language recommended  ";
            Result.Expl_Len := 40;
      end case;

      return Result;
   end Get_Recommendation;

   procedure Analyze_Patterns is
      Rec : Recommendation;
   begin
      Put_Line ("Learning Analysis Report");
      Put_Line ("========================");
      New_Line;

      for I in 1 .. Learn_DB.Count loop
         declare
            E : Learn_Entry renames Learn_DB.Entries (I);
         begin
            Rec := Get_Recommendation (E.Vuln_Type);

            Put ("Vulnerability: ");
            case E.Vuln_Type is
               when Buffer_Overflow   => Put_Line ("Buffer Overflow");
               when Null_Dereference  => Put_Line ("Null Dereference");
               when Race_Condition    => Put_Line ("Race Condition");
               when Injection_Attack  => Put_Line ("Injection Attack");
               when Use_After_Free    => Put_Line ("Use After Free");
               when Integer_Overflow  => Put_Line ("Integer Overflow");
               when Type_Confusion    => Put_Line ("Type Confusion");
               when Memory_Leak       => Put_Line ("Memory Leak");
               when Unvalidated_Input => Put_Line ("Unvalidated Input");
               when Other_Vuln        => Put_Line ("Other");
            end case;

            Put_Line ("  Occurrences: " & Natural'Image (E.Occurrences));
            Put_Line ("  Source Lang: " & Trim (E.Source_Lang (1 .. E.Lang_Len), Ada.Strings.Both));
            Put_Line ("  Suggested:   " & Trim (Rec.Languages (1 .. Rec.Lang_Len), Ada.Strings.Both));
            Put_Line ("  Confidence:  " & Natural'Image (Rec.Confidence) & "%");
            New_Line;

            --  Send notification for high-occurrence patterns
            if E.Occurrences >= 3 then
               Techstack_Notify.Send_Learning_Suggestion
                 (E.Vuln_Type,
                  Trim (E.Source_Lang (1 .. E.Lang_Len), Ada.Strings.Both),
                  Trim (Rec.Languages (1 .. Rec.Lang_Len), Ada.Strings.Both),
                  Rec.Confidence);
            end if;
         end;
      end loop;
   end Analyze_Patterns;

   procedure Load_Learning_DB (Path : String; Success : out Boolean) is
      pragma Unreferenced (Path);
   begin
      --  Would load from file
      Success := True;
   end Load_Learning_DB;

   procedure Save_Learning_DB (Path : String; Success : out Boolean) is
      File : File_Type;
   begin
      Success := False;
      Create (File, Out_File, Path);

      Put_Line (File, "# Techstack Learning Database");
      Put_Line (File, "# Auto-generated by learning mode");
      New_Line (File);

      for I in 1 .. Learn_DB.Count loop
         declare
            E : Learn_Entry renames Learn_DB.Entries (I);
         begin
            Put_Line (File, "[[pattern]]");
            case E.Vuln_Type is
               when Buffer_Overflow   => Put_Line (File, "vuln = ""buffer_overflow""");
               when Null_Dereference  => Put_Line (File, "vuln = ""null_deref""");
               when Race_Condition    => Put_Line (File, "vuln = ""race_condition""");
               when Injection_Attack  => Put_Line (File, "vuln = ""injection""");
               when Use_After_Free    => Put_Line (File, "vuln = ""use_after_free""");
               when Integer_Overflow  => Put_Line (File, "vuln = ""integer_overflow""");
               when Type_Confusion    => Put_Line (File, "vuln = ""type_confusion""");
               when Memory_Leak       => Put_Line (File, "vuln = ""memory_leak""");
               when Unvalidated_Input => Put_Line (File, "vuln = ""unvalidated_input""");
               when Other_Vuln        => Put_Line (File, "vuln = ""other""");
            end case;
            Put_Line (File, "lang = """ & Trim (E.Source_Lang (1 .. E.Lang_Len), Ada.Strings.Both) & """");
            Put_Line (File, "occurrences = " & Natural'Image (E.Occurrences));
            New_Line (File);
         end;
      end loop;

      Close (File);
      Success := True;

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         Success := False;
   end Save_Learning_DB;

   procedure Generate_Report (Output_Path : String) is
      File : File_Type;
      Rec  : Recommendation;
   begin
      Create (File, Out_File, Output_Path);

      Put_Line (File, "# Techstack Learning Report");
      Put_Line (File, "");
      Put_Line (File, "## Summary");
      Put_Line (File, "");
      Put_Line (File, "Total patterns learned: " & Natural'Image (Learn_DB.Count));
      Put_Line (File, "");
      Put_Line (File, "## Vulnerability Analysis");
      Put_Line (File, "");

      for I in 1 .. Learn_DB.Count loop
         declare
            E : Learn_Entry renames Learn_DB.Entries (I);
         begin
            Rec := Get_Recommendation (E.Vuln_Type);

            Put_Line (File, "### Pattern " & Natural'Image (I));
            Put_Line (File, "- **Vulnerability Type**: " &
              (case E.Vuln_Type is
                 when Buffer_Overflow   => "Buffer Overflow",
                 when Null_Dereference  => "Null Dereference",
                 when Race_Condition    => "Race Condition",
                 when Injection_Attack  => "Injection Attack",
                 when Use_After_Free    => "Use After Free",
                 when Integer_Overflow  => "Integer Overflow",
                 when Type_Confusion    => "Type Confusion",
                 when Memory_Leak       => "Memory Leak",
                 when Unvalidated_Input => "Unvalidated Input",
                 when Other_Vuln        => "Other"));
            Put_Line (File, "- **Occurrences**: " & Natural'Image (E.Occurrences));
            Put_Line (File, "- **Source Language**: " &
              Trim (E.Source_Lang (1 .. E.Lang_Len), Ada.Strings.Both));
            Put_Line (File, "- **Recommended**: " &
              Trim (Rec.Languages (1 .. Rec.Lang_Len), Ada.Strings.Both));
            Put_Line (File, "- **Confidence**: " & Natural'Image (Rec.Confidence) & "%");
            Put_Line (File, "");
         end;
      end loop;

      Close (File);

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
   end Generate_Report;

   procedure Reset_Learning is
   begin
      Learn_DB.Count := 0;
      Learn_DB.Entries := (others => Null_Learn_Entry);
   end Reset_Learning;

   procedure Auto_Suggest_Filter
     (Pattern   : out Pattern_String;
      Pat_Len   : out Natural;
      Level     : out Block_Level;
      Reason    : out Reason_String;
      Reas_Len  : out Natural;
      Suggested : out Boolean)
   is
      Best_Idx   : Natural := 0;
      Best_Count : Natural := 0;
   begin
      Pattern := (others => ' ');
      Pat_Len := 0;
      Level := Block;
      Reason := (others => ' ');
      Reas_Len := 0;
      Suggested := False;

      --  Find most frequent pattern not yet filtered
      for I in 1 .. Learn_DB.Count loop
         if Learn_DB.Entries (I).Occurrences > Best_Count and
            Learn_DB.Entries (I).Occurrences >= 3
         then
            Best_Count := Learn_DB.Entries (I).Occurrences;
            Best_Idx := I;
         end if;
      end loop;

      if Best_Idx > 0 then
         declare
            E : Learn_Entry renames Learn_DB.Entries (Best_Idx);
            Rec : constant Recommendation := Get_Recommendation (E.Vuln_Type);
         begin
            Pattern := E.File_Pattern;
            Pat_Len := E.Pattern_Len;
            Level := (if Best_Count >= 10 then Fatal else Block);
            Reason := Rec.Explanation;
            Reas_Len := Rec.Expl_Len;
            Suggested := True;
         end;
      end if;
   end Auto_Suggest_Filter;

end Techstack_Learning;
