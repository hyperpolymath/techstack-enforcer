-------------------------------------------------------------------------------
-- Techstack Enforcer - Definition Sets Implementation
-------------------------------------------------------------------------------

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Techstack_Enforcer;

package body Techstack_DefSets is

   --  Helper to create padded string
   function Pad (S : String; Len : Positive) return String is
      Result : String (1 .. Len) := (others => ' ');
   begin
      if S'Length <= Len then
         Result (1 .. S'Length) := S;
      end if;
      return Result;
   end Pad;

   function Get_DefSet_Info (ID : DefSet_ID) return DefSet_Info is
   begin
      case ID is
         when Strict =>
            return (ID           => Strict,
                    Name         => Pad ("strict", 32),
                    Name_Len     => 6,
                    Description  => Pad ("Maximum memory safety - blocks all unsafe languages", 128),
                    Desc_Len     => 51,
                    Filter_Count => 45);

         when Moderate =>
            return (ID           => Moderate,
                    Name         => Pad ("moderate", 32),
                    Name_Len     => 8,
                    Description  => Pad ("Balanced safety with practical exceptions", 128),
                    Desc_Len     => 41,
                    Filter_Count => 35);

         when Permissive =>
            return (ID           => Permissive,
                    Name         => Pad ("permissive", 32),
                    Name_Len     => 10,
                    Description  => Pad ("Advisory mode - warns but rarely blocks", 128),
                    Desc_Len     => 39,
                    Filter_Count => 20);

         when Enterprise =>
            return (ID           => Enterprise,
                    Name         => Pad ("enterprise", 32),
                    Name_Len     => 10,
                    Description  => Pad ("Corporate environments with legacy language support", 128),
                    Desc_Len     => 51,
                    Filter_Count => 40);

         when Memory_Safe =>
            return (ID           => Memory_Safe,
                    Name         => Pad ("memory_safe", 32),
                    Name_Len     => 11,
                    Description  => Pad ("Focus on memory safety - blocks C/C++/Assembly", 128),
                    Desc_Len     => 46,
                    Filter_Count => 25);

         when Custom =>
            return (ID           => Custom,
                    Name         => Pad ("custom", 32),
                    Name_Len     => 6,
                    Description  => Pad ("User-defined filters only (from config file)", 128),
                    Desc_Len     => 44,
                    Filter_Count => 0);
      end case;
   end Get_DefSet_Info;

   function Get_DefSet_Mode (ID : DefSet_ID) return Enforce_Mode is
   begin
      case ID is
         when Strict     => return Lockdown;
         when Moderate   => return Enforce;
         when Permissive => return Warn_Only;
         when Enterprise => return Enforce;
         when Memory_Safe => return Enforce;
         when Custom     => return Enforce;
      end case;
   end Get_DefSet_Mode;

   function Parse_DefSet_Name (Name : String) return DefSet_ID is
      Lower : String := Name;
   begin
      --  Convert to lowercase for comparison
      for I in Lower'Range loop
         if Lower (I) in 'A' .. 'Z' then
            Lower (I) := Character'Val (Character'Pos (Lower (I)) + 32);
         end if;
      end loop;

      if Lower = "strict" then
         return Strict;
      elsif Lower = "moderate" then
         return Moderate;
      elsif Lower = "permissive" then
         return Permissive;
      elsif Lower = "enterprise" then
         return Enterprise;
      elsif Lower = "memory_safe" or Lower = "memory-safe" or Lower = "memorysafe" then
         return Memory_Safe;
      else
         return Custom;
      end if;
   end Parse_DefSet_Name;

   function DefSet_To_String (ID : DefSet_ID) return String is
   begin
      case ID is
         when Strict     => return "strict";
         when Moderate   => return "moderate";
         when Permissive => return "permissive";
         when Enterprise => return "enterprise";
         when Memory_Safe => return "memory_safe";
         when Custom     => return "custom";
      end case;
   end DefSet_To_String;

   procedure Load_Strict_Filters is
      Success : Boolean;
   begin
      --  FATAL: All memory-unsafe and dynamically typed languages
      Techstack_Enforcer.Add_Filter ("*.py", Fatal, "Python: memory-unsafe, no static typing", Success);
      Techstack_Enforcer.Add_Filter ("*.pyc", Fatal, "Python bytecode", Success);
      Techstack_Enforcer.Add_Filter ("*.pyo", Fatal, "Python optimized bytecode", Success);
      Techstack_Enforcer.Add_Filter ("requirements.txt", Fatal, "Python dependencies", Success);
      Techstack_Enforcer.Add_Filter ("Pipfile", Fatal, "Python Pipenv", Success);
      Techstack_Enforcer.Add_Filter ("pyproject.toml", Fatal, "Python project", Success);
      Techstack_Enforcer.Add_Filter ("*.php", Fatal, "PHP: memory-unsafe, injection-prone", Success);
      Techstack_Enforcer.Add_Filter ("composer.json", Fatal, "PHP Composer", Success);
      Techstack_Enforcer.Add_Filter ("*.rb", Fatal, "Ruby: memory-unsafe runtime", Success);
      Techstack_Enforcer.Add_Filter ("Gemfile", Fatal, "Ruby dependencies", Success);
      Techstack_Enforcer.Add_Filter ("*.pl", Fatal, "Perl: memory-unsafe", Success);
      Techstack_Enforcer.Add_Filter ("*.pm", Fatal, "Perl module", Success);

      --  BLOCK: Memory-unsafe compiled languages
      Techstack_Enforcer.Add_Filter ("*.c", Block, "C: manual memory management", Success);
      Techstack_Enforcer.Add_Filter ("*.cpp", Block, "C++: manual memory management", Success);
      Techstack_Enforcer.Add_Filter ("*.cc", Block, "C++: manual memory management", Success);
      Techstack_Enforcer.Add_Filter ("*.cxx", Block, "C++: manual memory management", Success);
      Techstack_Enforcer.Add_Filter ("*.h", Block, "C/C++ header", Success);
      Techstack_Enforcer.Add_Filter ("*.hpp", Block, "C++ header", Success);
      Techstack_Enforcer.Add_Filter ("*.asm", Block, "Assembly: no safety guarantees", Success);
      Techstack_Enforcer.Add_Filter ("*.s", Block, "Assembly source", Success);

      --  BLOCK: JavaScript ecosystem (use TypeScript)
      Techstack_Enforcer.Add_Filter ("*.js", Block, "JavaScript: use TypeScript", Success);
      Techstack_Enforcer.Add_Filter ("*.jsx", Block, "JSX: use TSX", Success);
      Techstack_Enforcer.Add_Filter ("*.mjs", Block, "ES modules: use TypeScript", Success);
      Techstack_Enforcer.Add_Filter ("*.cjs", Block, "CommonJS: use TypeScript", Success);

      --  BLOCK: Vendor lock-in
      Techstack_Enforcer.Add_Filter ("Dockerfile", Block, "Use Containerfile (Podman)", Success);
      Techstack_Enforcer.Add_Filter ("docker-compose.yml", Block, "Use podman-compose", Success);
      Techstack_Enforcer.Add_Filter ("docker-compose.yaml", Block, "Use podman-compose", Success);
      Techstack_Enforcer.Add_Filter (".github/**", Block, "Use GitLab CI", Success);

      --  WARN: Shell scripts (necessary evil)
      Techstack_Enforcer.Add_Filter ("*.sh", Warn, "Shell script - verify manually", Success);
      Techstack_Enforcer.Add_Filter ("*.bash", Warn, "Bash script - verify manually", Success);

      --  ALLOW: Memory-safe languages (whitelist)
      Techstack_Enforcer.Add_Filter ("*.rs", Allow, "Rust: memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("Cargo.toml", Allow, "Rust project", Success);
      Techstack_Enforcer.Add_Filter ("*.adb", Allow, "Ada: formally verifiable", Success);
      Techstack_Enforcer.Add_Filter ("*.ads", Allow, "Ada specification", Success);
      Techstack_Enforcer.Add_Filter ("*.hs", Allow, "Haskell: pure functional", Success);
      Techstack_Enforcer.Add_Filter ("*.ml", Allow, "OCaml: type-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.mli", Allow, "OCaml interface", Success);
      Techstack_Enforcer.Add_Filter ("*.elm", Allow, "Elm: no runtime errors", Success);
      Techstack_Enforcer.Add_Filter ("*.ex", Allow, "Elixir: fault-tolerant", Success);
      Techstack_Enforcer.Add_Filter ("*.exs", Allow, "Elixir script", Success);
      Techstack_Enforcer.Add_Filter ("*.erl", Allow, "Erlang: fault-tolerant", Success);
      Techstack_Enforcer.Add_Filter ("*.res", Allow, "ReScript: type-safe JS", Success);
      Techstack_Enforcer.Add_Filter ("*.nix", Allow, "Nix: reproducible builds", Success);
   end Load_Strict_Filters;

   procedure Load_Moderate_Filters is
      Success : Boolean;
   begin
      --  FATAL: Only the most problematic languages
      Techstack_Enforcer.Add_Filter ("*.py", Fatal, "Python: use typed alternative", Success);
      Techstack_Enforcer.Add_Filter ("*.php", Fatal, "PHP: security concerns", Success);

      --  BLOCK: Memory-unsafe core languages
      Techstack_Enforcer.Add_Filter ("*.c", Block, "C: prefer Rust or Zig", Success);
      Techstack_Enforcer.Add_Filter ("*.cpp", Block, "C++: prefer Rust", Success);
      Techstack_Enforcer.Add_Filter ("*.cc", Block, "C++: prefer Rust", Success);

      --  WARN: Dynamic languages and vendor lock-in
      Techstack_Enforcer.Add_Filter ("*.js", Warn, "JavaScript: consider TypeScript", Success);
      Techstack_Enforcer.Add_Filter ("*.jsx", Warn, "JSX: consider TSX", Success);
      Techstack_Enforcer.Add_Filter ("*.rb", Warn, "Ruby: consider typed alternative", Success);
      Techstack_Enforcer.Add_Filter ("*.pl", Warn, "Perl: legacy language", Success);
      Techstack_Enforcer.Add_Filter ("*.h", Warn, "C header - review for safety", Success);
      Techstack_Enforcer.Add_Filter ("*.hpp", Warn, "C++ header - review for safety", Success);
      Techstack_Enforcer.Add_Filter ("Dockerfile", Warn, "Consider Containerfile", Success);
      Techstack_Enforcer.Add_Filter (".github/**", Warn, "Consider GitLab CI", Success);

      --  ALLOW: Safe and practical languages
      Techstack_Enforcer.Add_Filter ("*.rs", Allow, "Rust: memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.go", Allow, "Go: GC-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.ts", Allow, "TypeScript: typed JS", Success);
      Techstack_Enforcer.Add_Filter ("*.tsx", Allow, "TSX: typed React", Success);
      Techstack_Enforcer.Add_Filter ("*.hs", Allow, "Haskell: pure functional", Success);
      Techstack_Enforcer.Add_Filter ("*.adb", Allow, "Ada: formally verifiable", Success);
      Techstack_Enforcer.Add_Filter ("*.ads", Allow, "Ada specification", Success);
      Techstack_Enforcer.Add_Filter ("*.ex", Allow, "Elixir: fault-tolerant", Success);
      Techstack_Enforcer.Add_Filter ("*.elm", Allow, "Elm: no runtime errors", Success);
      Techstack_Enforcer.Add_Filter ("*.nix", Allow, "Nix: reproducible", Success);
      Techstack_Enforcer.Add_Filter ("*.zig", Allow, "Zig: safe low-level", Success);
   end Load_Moderate_Filters;

   procedure Load_Permissive_Filters is
      Success : Boolean;
   begin
      --  WARN only: Advisory messages
      Techstack_Enforcer.Add_Filter ("*.py", Warn, "Python: consider typed alternative", Success);
      Techstack_Enforcer.Add_Filter ("*.php", Warn, "PHP: review security practices", Success);
      Techstack_Enforcer.Add_Filter ("*.c", Warn, "C: manual memory - use with care", Success);
      Techstack_Enforcer.Add_Filter ("*.cpp", Warn, "C++: consider modern alternatives", Success);
      Techstack_Enforcer.Add_Filter ("*.js", Warn, "JavaScript: consider TypeScript", Success);
      Techstack_Enforcer.Add_Filter ("*.rb", Warn, "Ruby: consider typed variant", Success);
      Techstack_Enforcer.Add_Filter ("Dockerfile", Warn, "Docker: consider Podman", Success);

      --  ALLOW: Everything else passes
      Techstack_Enforcer.Add_Filter ("*.rs", Allow, "Rust: excellent choice", Success);
      Techstack_Enforcer.Add_Filter ("*.go", Allow, "Go: good for services", Success);
      Techstack_Enforcer.Add_Filter ("*.ts", Allow, "TypeScript: typed JS", Success);
      Techstack_Enforcer.Add_Filter ("*.java", Allow, "Java: enterprise standard", Success);
      Techstack_Enforcer.Add_Filter ("*.kt", Allow, "Kotlin: modern JVM", Success);
      Techstack_Enforcer.Add_Filter ("*.scala", Allow, "Scala: functional JVM", Success);
      Techstack_Enforcer.Add_Filter ("*.hs", Allow, "Haskell: pure functional", Success);
      Techstack_Enforcer.Add_Filter ("*.ex", Allow, "Elixir: fault-tolerant", Success);
   end Load_Permissive_Filters;

   procedure Load_Enterprise_Filters is
      Success : Boolean;
   begin
      --  BLOCK: High-risk for enterprise
      Techstack_Enforcer.Add_Filter ("*.py", Block, "Python: use approved languages", Success);
      Techstack_Enforcer.Add_Filter ("*.php", Block, "PHP: not approved", Success);
      Techstack_Enforcer.Add_Filter ("*.rb", Block, "Ruby: not in enterprise stack", Success);
      Techstack_Enforcer.Add_Filter ("*.pl", Block, "Perl: legacy not maintained", Success);

      --  WARN: Legacy but tolerated
      Techstack_Enforcer.Add_Filter ("*.c", Warn, "C: legacy - migration planned", Success);
      Techstack_Enforcer.Add_Filter ("*.cpp", Warn, "C++: legacy - migration planned", Success);
      Techstack_Enforcer.Add_Filter ("*.js", Warn, "JavaScript: prefer TypeScript", Success);
      Techstack_Enforcer.Add_Filter ("*.sh", Warn, "Shell: review for secrets", Success);
      Techstack_Enforcer.Add_Filter ("Dockerfile", Warn, "Docker: approved with review", Success);
      Techstack_Enforcer.Add_Filter (".github/**", Warn, "GitHub: prefer internal CI", Success);

      --  ALLOW: Approved enterprise languages
      Techstack_Enforcer.Add_Filter ("*.java", Allow, "Java: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.kt", Allow, "Kotlin: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.go", Allow, "Go: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.rs", Allow, "Rust: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.ts", Allow, "TypeScript: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.tsx", Allow, "TSX: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.cs", Allow, "C#: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.scala", Allow, "Scala: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.hs", Allow, "Haskell: enterprise approved", Success);
      Techstack_Enforcer.Add_Filter ("*.adb", Allow, "Ada: safety-critical approved", Success);
      Techstack_Enforcer.Add_Filter ("*.ads", Allow, "Ada specification", Success);
   end Load_Enterprise_Filters;

   procedure Load_Memory_Safe_Filters is
      Success : Boolean;
   begin
      --  BLOCK: Languages without memory safety
      Techstack_Enforcer.Add_Filter ("*.c", Block, "C: no memory safety", Success);
      Techstack_Enforcer.Add_Filter ("*.cpp", Block, "C++: no memory safety", Success);
      Techstack_Enforcer.Add_Filter ("*.cc", Block, "C++: no memory safety", Success);
      Techstack_Enforcer.Add_Filter ("*.cxx", Block, "C++: no memory safety", Success);
      Techstack_Enforcer.Add_Filter ("*.h", Block, "C header: memory-unsafe", Success);
      Techstack_Enforcer.Add_Filter ("*.hpp", Block, "C++ header: memory-unsafe", Success);
      Techstack_Enforcer.Add_Filter ("*.asm", Block, "Assembly: no safety", Success);
      Techstack_Enforcer.Add_Filter ("*.s", Block, "Assembly: no safety", Success);
      Techstack_Enforcer.Add_Filter ("*.S", Block, "Assembly: no safety", Success);

      --  WARN: Languages with partial safety
      Techstack_Enforcer.Add_Filter ("*.d", Warn, "D: optional GC - verify usage", Success);
      Techstack_Enforcer.Add_Filter ("*.nim", Warn, "Nim: verify memory mode", Success);
      Techstack_Enforcer.Add_Filter ("*.zig", Warn, "Zig: manual memory - verify", Success);

      --  ALLOW: Memory-safe languages
      Techstack_Enforcer.Add_Filter ("*.rs", Allow, "Rust: ownership-based safety", Success);
      Techstack_Enforcer.Add_Filter ("*.adb", Allow, "Ada: formally verifiable", Success);
      Techstack_Enforcer.Add_Filter ("*.ads", Allow, "Ada: formally verifiable", Success);
      Techstack_Enforcer.Add_Filter ("*.hs", Allow, "Haskell: GC + pure", Success);
      Techstack_Enforcer.Add_Filter ("*.ml", Allow, "OCaml: GC + type-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.go", Allow, "Go: GC memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.java", Allow, "Java: GC memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.kt", Allow, "Kotlin: GC memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.scala", Allow, "Scala: GC memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.py", Allow, "Python: GC memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.rb", Allow, "Ruby: GC memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.ex", Allow, "Elixir: BEAM memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.erl", Allow, "Erlang: BEAM memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.js", Allow, "JavaScript: GC memory-safe", Success);
      Techstack_Enforcer.Add_Filter ("*.ts", Allow, "TypeScript: GC memory-safe", Success);
   end Load_Memory_Safe_Filters;

   procedure Load_DefSet (ID : DefSet_ID; Success : out Boolean) is
   begin
      Success := True;

      --  Re-initialize to clear existing filters
      Techstack_Enforcer.Initialize;

      --  Set the appropriate mode
      Techstack_Enforcer.Set_Mode (Get_DefSet_Mode (ID));

      --  Load the specific filter set
      case ID is
         when Strict     => Load_Strict_Filters;
         when Moderate   => Load_Moderate_Filters;
         when Permissive => Load_Permissive_Filters;
         when Enterprise => Load_Enterprise_Filters;
         when Memory_Safe => Load_Memory_Safe_Filters;
         when Custom     => null;  -- Keep default initialization
      end case;

   exception
      when others =>
         Success := False;
   end Load_DefSet;

   procedure Apply_DefSet (ID : DefSet_ID; Success : out Boolean) is
   begin
      Success := True;

      --  Don't reinitialize - just add to existing filters
      case ID is
         when Strict     => Load_Strict_Filters;
         when Moderate   => Load_Moderate_Filters;
         when Permissive => Load_Permissive_Filters;
         when Enterprise => Load_Enterprise_Filters;
         when Memory_Safe => Load_Memory_Safe_Filters;
         when Custom     => null;
      end case;

   exception
      when others =>
         Success := False;
   end Apply_DefSet;

   procedure List_DefSets is
   begin
      Put_Line ("Available Definition Sets:");
      Put_Line ("----------------------------------------");

      for ID in DefSet_ID loop
         declare
            Info : constant DefSet_Info := Get_DefSet_Info (ID);
         begin
            Put ("  ");
            Put (Head (Trim (Info.Name (1 .. Info.Name_Len), Ada.Strings.Both), 12));
            Put (" - ");
            Put_Line (Trim (Info.Description (1 .. Info.Desc_Len), Ada.Strings.Both));
         end;
      end loop;

      New_Line;
      Put_Line ("Usage: techstack-enforcer --defset=<name> <command>");
   end List_DefSets;

   procedure Export_DefSet_TOML
     (ID       : DefSet_ID;
      Filename : String;
      Success  : out Boolean)
   is
      File : File_Type;
      Info : constant DefSet_Info := Get_DefSet_Info (ID);
   begin
      Success := False;

      Create (File, Out_File, Filename);

      Put_Line (File, "# Techstack Definition Set: " &
                Trim (Info.Name (1 .. Info.Name_Len), Ada.Strings.Both));
      Put_Line (File, "# " & Trim (Info.Description (1 .. Info.Desc_Len), Ada.Strings.Both));
      Put_Line (File, "# Generated by techstack-enforcer");
      New_Line (File);

      Put_Line (File, "[defset]");
      Put_Line (File, "name = """ & Trim (Info.Name (1 .. Info.Name_Len), Ada.Strings.Both) & """");
      Put_Line (File, "mode = """ & DefSet_To_String (ID) & """");
      New_Line (File);

      Put_Line (File, "[mode]");
      case Get_DefSet_Mode (ID) is
         when Learning  => Put_Line (File, "enforce = ""learning""");
         when Warn_Only => Put_Line (File, "enforce = ""warn""");
         when Enforce   => Put_Line (File, "enforce = ""enforce""");
         when Lockdown  => Put_Line (File, "enforce = ""lockdown""");
      end case;
      New_Line (File);

      Put_Line (File, "# Filter definitions follow...");
      Put_Line (File, "# Load this set with: techstack-enforcer --defset=" &
                Trim (Info.Name (1 .. Info.Name_Len), Ada.Strings.Both));

      Close (File);
      Success := True;

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         Success := False;
   end Export_DefSet_TOML;

end Techstack_DefSets;
